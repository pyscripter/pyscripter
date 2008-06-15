inherited AskParamForm: TAskParamForm
  Left = 262
  Top = 241
  BorderIcons = [biSystemMenu]
  Caption = 'Unknown parameter'
  ClientHeight = 113
  ClientWidth = 385
  Font.Name = 'MS Shell Dlg 2'
  Position = poScreenCenter
  ExplicitWidth = 391
  ExplicitHeight = 139
  DesignSize = (
    385
    113)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TSpTBXButton
    Left = 221
    Top = 80
    Width = 75
    Height = 25
    Caption = '&OK'
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
  end
  object btnCancel: TSpTBXButton
    Left = 302
    Top = 80
    Width = 75
    Height = 25
    Caption = '&Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object chkSaveToFile: TSpTBXCheckBox
    Left = 8
    Top = 55
    Width = 184
    Height = 15
    Caption = 'and save it as a custom parameter'
    ParentColor = True
    TabOrder = 1
    ThemeType = thtWindows
  end
  object txtParamValue: TSpTBXEdit
    Left = 8
    Top = 24
    Width = 367
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ThemeType = thtWindows
  end
  object Label1: TSpTBXLabel
    Left = 8
    Top = 8
    Width = 128
    Height = 13
    Caption = 'Enter value for parameter '
    ParentColor = True
    FocusControl = txtParamValue
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
end
