inherited RemoteFileDialog: TRemoteFileDialog
  HelpContext = 580
  ClientHeight = 130
  ClientWidth = 434
  OnCloseQuery = FormCloseQuery
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 434
    Height = 93
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 48
      Height = 15
      Caption = 'File Path:'
    end
    object Label2: TLabel
      Left = 16
      Top = 47
      Width = 59
      Height = 15
      Caption = 'SSH Server:'
    end
    object edFileName: TEdit
      Left = 144
      Top = 13
      Width = 265
      Height = 23
      TabOrder = 0
    end
    object cbSSHConfigs: TComboBox
      Left = 144
      Top = 45
      Width = 235
      Height = 23
      Style = csDropDownList
      TabOrder = 1
    end
    object btnSSHServersSetup: TButton
      Left = 385
      Top = 43
      Width = 24
      Height = 24
      ImageAlignment = iaCenter
      ImageIndex = 0
      ImageName = 'Setup'
      Images = vilImages
      TabOrder = 2
      OnClick = btnSSHServersSetupClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 93
    Width = 434
    Height = 37
    Align = alBottom
    Anchors = [akLeft, akBottom]
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      434
      37)
    object OKButton: TButton
      Left = 143
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 239
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 335
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 103
        CollectionName = 'Setup'
        Name = 'Setup'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 16
    Top = 80
  end
end
