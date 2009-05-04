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
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 113
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      385
      113)
    object btnOK: TSpTBXButton
      Left = 221
      Top = 80
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Default = True
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
      ModalResult = 2
    end
    object chkSaveToFile: TSpTBXCheckBox
      Left = 8
      Top = 55
      Width = 190
      Height = 21
      Caption = 'and save it as a custom parameter'
      ParentColor = True
      TabOrder = 1
    end
    object txtParamValue: TSpTBXEdit
      Left = 8
      Top = 24
      Width = 367
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object Label1: TSpTBXLabel
      Left = 8
      Top = 2
      Width = 134
      Height = 19
      Caption = 'Enter value for parameter '
      FocusControl = txtParamValue
    end
  end
end
