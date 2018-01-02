inherited ImportDirectoryForm: TImportDirectoryForm
  Caption = 'Import Directory'
  ClientHeight = 145
  ClientWidth = 436
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  ExplicitWidth = 442
  ExplicitHeight = 173
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 145
    Color = clBtnFace
    Align = alClient
    UseDockManager = True
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      436
      145)
    object ebMask: TEdit
      Left = 167
      Top = 72
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
      Left = 349
      Top = 74
      Width = 76
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Recursive?'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object Label1: TSpTBXLabel
      Left = 8
      Top = 16
      Width = 100
      Height = 19
      Caption = 'Directory to import:'
    end
    object Label2: TSpTBXLabel
      Left = 8
      Top = 72
      Width = 148
      Height = 19
      Caption = 'File Masks (e.g. *.py;*.pyw):'
    end
    object Button1: TSpTBXButton
      Left = 111
      Top = 110
      Width = 80
      Height = 25
      Caption = '&OK'
      Anchors = [akLeft, akBottom]
      TabOrder = 1
      Default = True
      ModalResult = 1
    end
    object Button2: TSpTBXButton
      Left = 215
      Top = 110
      Width = 80
      Height = 25
      Caption = '&Cancel'
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      Cancel = True
      ModalResult = 2
    end
    object DirectoryEdit: TSpTBXButtonEdit
      Left = 8
      Top = 38
      Width = 417
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      EditButton.Left = 394
      EditButton.Top = 0
      EditButton.Width = 19
      EditButton.Height = 17
      EditButton.Align = alRight
      EditButton.OnClick = DirectoryEditBtnClick
      EditButton.Images = CommandsDataModule.Images
      EditButton.ImageIndex = 45
    end
  end
end
