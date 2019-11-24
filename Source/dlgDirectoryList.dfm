inherited DirectoryListDialog: TDirectoryListDialog
  Left = 206
  Top = 99
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Directory list'
  ClientHeight = 342
  ClientWidth = 379
  Constraints.MinHeight = 208
  Constraints.MinWidth = 358
  OldCreateOrder = True
  OnShow = FormShow
  ExplicitWidth = 395
  ExplicitHeight = 381
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 379
    Height = 232
    Align = alClient
    TabOrder = 0
    DesignSize = (
      379
      232)
    object Label1: TLabel
      Left = 10
      Top = 9
      Width = 120
      Height = 13
      Caption = 'Ordered list of file paths:'
      Color = clNone
      ParentColor = False
    end
    object btnMoveUp: TButton
      Left = 345
      Top = 96
      Width = 28
      Height = 24
      Hint = 'Move up'
      Anchors = [akTop]
      ImageAlignment = iaCenter
      ImageIndex = 1
      Images = vilImages
      TabOrder = 1
      OnClick = btnMoveUpClick
    end
    object btnMoveDown: TButton
      Left = 345
      Top = 134
      Width = 28
      Height = 24
      Hint = 'Move down'
      Anchors = [akTop]
      ImageAlignment = iaCenter
      ImageIndex = 2
      Images = vilImages
      TabOrder = 2
      OnClick = btnMoveDownClick
    end
    object DirectoryList: TListBox
      Left = 10
      Top = 34
      Width = 332
      Height = 189
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 0
      OnClick = DirectoryListClick
      OnDragDrop = DirectoryListDragDrop
      OnDragOver = DirectoryListDragOver
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 232
    Width = 379
    Height = 110
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      379
      110)
    object Panel3: TPanel
      Left = 1
      Top = 67
      Width = 377
      Height = 42
      Align = alBottom
      TabOrder = 0
      DesignSize = (
        377
        42)
      object OKBtn: TButton
        Left = 202
        Top = 10
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object CancelBtn: TButton
        Left = 290
        Top = 10
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Cancel = True
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
    object btnAdd: TButton
      Left = 44
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Add file path to the list'
      Caption = '&Add...'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnReplace: TButton
      Left = 136
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Replace the selected file path'
      Caption = '&Replace'
      TabOrder = 2
      OnClick = btnReplaceClick
    end
    object btnDelete: TButton
      Left = 228
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Delete the selected file path'
      Caption = '&Delete'
      TabOrder = 4
      OnClick = btnDeleteClick
    end
    object edPath: TButtonedEdit
      Left = 10
      Top = 6
      Width = 332
      Height = 21
      Hint = 'Enter file path here'
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      Images = vilImages
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 3
      OnChange = edPathChange
      OnRightButtonClick = BtnPathClick
    end
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 45
        CollectionName = 'Item46'
        Disabled = False
        Name = 'Item46'
      end
      item
        CollectionIndex = 47
        CollectionName = 'Item48'
        Disabled = False
        Name = 'Item48'
      end
      item
        CollectionIndex = 48
        CollectionName = 'Item49'
        Disabled = False
        Name = 'Item49'
      end
      item
        CollectionIndex = 84
        CollectionName = 'Item85'
        Disabled = False
        Name = 'Item85'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 40
    Top = 120
  end
end
