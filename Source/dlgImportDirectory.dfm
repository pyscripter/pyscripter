inherited ImportDirectoryForm: TImportDirectoryForm
  Caption = 'Import Directory'
  ClientHeight = 145
  ClientWidth = 469
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 469
    Height = 145
    Align = alClient
    TabOrder = 0
    DesignSize = (
      469
      145)
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
      Width = 142
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbRecursive: TCheckBox
      Left = 352
      Top = 71
      Width = 108
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Recursive?'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object Button1: TButton
      Left = 124
      Top = 111
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object Button2: TButton
      Left = 228
      Top = 111
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object DirectoryEdit: TButtonedEdit
      Left = 8
      Top = 38
      Width = 452
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      Images = vilImages
      RightButton.ImageIndex = 0
      RightButton.ImageName = 'OpenFolder'
      RightButton.Visible = True
      TabOrder = 4
      OnRightButtonClick = DirectoryEditBtnClick
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 63
        CollectionName = 'OpenFolder'
        Name = 'OpenFolder'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 24
    Top = 93
  end
end
