object ImportDirectoryForm: TImportDirectoryForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Import Directory'
  ClientHeight = 142
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 324
    Height = 142
    Align = alClient
    TabOrder = 0
    DesignSize = (
      324
      142)
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 94
      Height = 13
      Caption = 'Directory to import:'
    end
    object Label2: TLabel
      Left = 8
      Top = 72
      Width = 103
      Height = 13
      Caption = 'File Mask (e.g. *.py):'
    end
    object DirectoryEdit: TJvDirectoryEdit
      Left = 8
      Top = 35
      Width = 305
      Height = 21
      DialogKind = dkWin32
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object ebMask: TEdit
      Left = 122
      Top = 69
      Width = 52
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = 'adf'
    end
    object Button1: TButton
      Left = 71
      Top = 103
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object Button2: TButton
      Left = 175
      Top = 103
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object cbRecursive: TCheckBox
      Left = 237
      Top = 71
      Width = 76
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Recursive?'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
end
