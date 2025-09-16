inherited ImportDirectoryForm: TImportDirectoryForm
  HelpContext = 435
  Caption = 'Import Directory'
  ClientHeight = 208
  ClientWidth = 485
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  DesignSize = (
    485
    208)
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 165
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      485
      165)
    object Label1: TLabel
      Left = 8
      Top = 15
      Width = 104
      Height = 15
      Caption = 'Directory to import:'
      Color = clNone
      ParentColor = False
    end
    object Label2: TLabel
      Left = 8
      Top = 72
      Width = 144
      Height = 15
      Caption = 'File Masks (e.g. *.py;*.pyw):'
      Color = clNone
      ParentColor = False
    end
    object ebMask: TEdit
      Left = 190
      Top = 69
      Width = 286
      Height = 23
      TabOrder = 0
    end
    object cbRecursive: TCheckBox
      Left = 8
      Top = 93
      Width = 328
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Recursive'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object DirectoryEdit: TButtonedEdit
      Left = 8
      Top = 38
      Width = 468
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      Images = vilImages
      RightButton.ImageIndex = 0
      RightButton.ImageName = 'OpenFolder'
      RightButton.Visible = True
      TabOrder = 2
      OnRightButtonClick = DirectoryEditBtnClick
    end
    object cbAutoUpdate: TCheckBox
      Left = 8
      Top = 116
      Width = 328
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Auto-update'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbPythonPath: TCheckBox
      Left = 8
      Top = 139
      Width = 328
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add to python path'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object Button1: TButton
    Left = 152
    Top = 176
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 256
    Top = 176
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 72
        CollectionName = 'OpenFolder'
        Name = 'OpenFolder'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 432
    Top = 93
  end
end
