inherited DirectoryListDialog: TDirectoryListDialog
  Left = 206
  Top = 99
  BorderIcons = [biSystemMenu]
  Caption = 'Directory list'
  ClientHeight = 352
  ClientWidth = 389
  Constraints.MinHeight = 208
  Constraints.MinWidth = 358
  Font.Name = 'MS Shell Dlg 2'
  OldCreateOrder = True
  OnShow = FormShow
  ExplicitWidth = 395
  ExplicitHeight = 378
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 389
    Height = 242
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      389
      242)
    object DirectoryList: TSpTBXListBox
      Left = 10
      Top = 34
      Width = 342
      Height = 199
      Style = lbStandard
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 0
      OnClick = DirectoryListClick
      OnDragDrop = DirectoryListDragDrop
      OnDragOver = DirectoryListDragOver
    end
    object btnMoveUp: TSpTBXButton
      Left = 354
      Top = 96
      Width = 28
      Height = 24
      Hint = 'Move up'
      Anchors = [akTop]
      TabOrder = 1
      OnClick = btnMoveUpClick
      Images = CommandsDataModule.Images
      ImageIndex = 47
    end
    object btnMoveDown: TSpTBXButton
      Left = 354
      Top = 134
      Width = 28
      Height = 24
      Hint = 'Move down'
      Anchors = [akTop]
      TabOrder = 2
      OnClick = btnMoveDownClick
      Images = CommandsDataModule.Images
      ImageIndex = 48
    end
    object SpTBXLabel1: TSpTBXLabel
      Left = 8
      Top = 15
      Width = 126
      Height = 19
      Caption = 'Ordered list of file paths:'
    end
  end
  object SpTBXPanel2: TSpTBXPanel
    Left = 0
    Top = 242
    Width = 389
    Height = 110
    Caption = 'SpTBXPanel2'
    Align = alBottom
    TabOrder = 1
    BorderType = pbrFramed
    TBXStyleBackground = True
    DesignSize = (
      389
      110)
    object SpTBXPanel3: TSpTBXPanel
      Left = 2
      Top = 66
      Width = 385
      Height = 42
      Caption = 'SpTBXPanel3'
      Align = alBottom
      TabOrder = 0
      TBXStyleBackground = True
      DesignSize = (
        385
        42)
      object OKBtn: TSpTBXButton
        Left = 210
        Top = 10
        Width = 75
        Height = 25
        Caption = '&OK'
        Anchors = [akTop, akRight]
        TabOrder = 0
        Default = True
        ModalResult = 1
      end
      object CancelBtn: TSpTBXButton
        Left = 298
        Top = 10
        Width = 75
        Height = 25
        Caption = '&Cancel'
        Anchors = [akTop, akRight]
        TabOrder = 1
        Cancel = True
        ModalResult = 2
      end
    end
    object btnAdd: TSpTBXButton
      Left = 44
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Add file path to the list'
      Caption = '&Add...'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnReplace: TSpTBXButton
      Left = 136
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Replace the selected file path'
      Caption = '&Replace'
      TabOrder = 2
      OnClick = btnReplaceClick
    end
    object edPath: TSpTBXButtonEdit
      Left = 10
      Top = 6
      Width = 342
      Height = 21
      Hint = 'Enter file path here'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edPathChange
      EditButton.Left = 318
      EditButton.Top = 0
      EditButton.Width = 20
      EditButton.Height = 17
      EditButton.Hint = 'Select file path'
      EditButton.Align = alRight
      EditButton.OnClick = BtnPathClick
      EditButton.Images = CommandsDataModule.Images
      EditButton.ImageIndex = 45
    end
    object btnDelete: TSpTBXButton
      Left = 228
      Top = 33
      Width = 75
      Height = 25
      Hint = 'Delete the selected file path'
      Caption = '&Delete'
      TabOrder = 4
      OnClick = btnDeleteClick
    end
  end
end
