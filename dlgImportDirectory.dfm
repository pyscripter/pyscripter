inherited ImportDirectoryForm: TImportDirectoryForm
  Caption = 'Import Directory'
  ClientHeight = 160
  ClientWidth = 412
  Font.Name = 'MS Shell Dlg 2'
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  ExplicitWidth = 418
  ExplicitHeight = 186
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 412
    Height = 160
    ThemeType = thtWindows
    Align = alClient
    Color = clBtnFace
    UseDockManager = True
    ParentColor = False
    TabOrder = 0
    ExplicitTop = 2
    DesignSize = (
      412
      160)
    object DirectoryEdit: TJvDirectoryEdit
      Left = 8
      Top = 35
      Width = 393
      Height = 21
      DialogKind = dkWin32
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object ebMask: TEdit
      Left = 8
      Top = 91
      Width = 142
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object cbRecursive: TCheckBox
      Left = 215
      Top = 72
      Width = 76
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Recursive?'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object Label1: TSpTBXLabel
      Left = 8
      Top = 16
      Width = 94
      Height = 13
      Caption = 'Directory to import:'
      ParentColor = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object Label2: TSpTBXLabel
      Left = 8
      Top = 72
      Width = 142
      Height = 13
      Caption = 'File Masks (e.g. *.py;*.pyw):'
      ParentColor = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object Button1: TSpTBXButton
      Left = 111
      Top = 125
      Width = 80
      Height = 25
      Caption = '&OK'
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 1
      ExplicitTop = 107
    end
    object Button2: TSpTBXButton
      Left = 215
      Top = 125
      Width = 80
      Height = 25
      Caption = '&Cancel'
      Anchors = [akLeft, akBottom]
      TabOrder = 3
      Cancel = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
      ExplicitTop = 107
    end
  end
end
