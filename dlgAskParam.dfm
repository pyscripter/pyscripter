object AskParamForm: TAskParamForm
  Left = 262
  Top = 241
  BorderIcons = [biSystemMenu]
  Caption = 'Unknown parameter'
  ClientHeight = 113
  ClientWidth = 385
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
  object btnOK: TSpTBXButton
    Left = 222
    Top = 82
    Width = 75
    Height = 25
    Caption = 'OK'
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
  end
  object btnCancel: TSpTBXButton
    Left = 302
    Top = 82
    Width = 75
    Height = 25
    Caption = 'Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object chkSaveToFile: TSpTBXCheckBox
    Left = 8
    Top = 73
    Width = 166
    Height = 15
    Caption = 'and save it a custom parameter'
    ParentColor = True
    TabOrder = 1
    ThemeType = thtWindows
  end
end
