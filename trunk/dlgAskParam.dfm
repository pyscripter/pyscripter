object AskParamForm: TAskParamForm
  Left = 262
  Top = 241
  Width = 393
  Height = 147
  BorderIcons = [biSystemMenu]
  Caption = 'Unknown parameter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    385
    113)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 122
    Height = 13
    Caption = 'Enter value for parameter '
    FocusControl = txtParamValue
  end
  object txtParamValue: TEdit
    Left = 8
    Top = 24
    Width = 367
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object chkSaveToFile: TCheckBox
    Left = 8
    Top = 73
    Width = 186
    Height = 17
    Caption = 'and save it a custom parameter'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 222
    Top = 82
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 302
    Top = 82
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
