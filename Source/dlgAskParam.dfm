inherited AskParamForm: TAskParamForm
  Left = 262
  Top = 241
  BorderIcons = [biSystemMenu]
  Caption = 'Unknown parameter'
  ClientHeight = 113
  ClientWidth = 385
  Position = poScreenCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 113
    Align = alClient
    TabOrder = 0
    DesignSize = (
      385
      113)
    object Label1: TLabel
      Left = 8
      Top = 5
      Width = 136
      Height = 15
      Caption = 'Enter value for parameter '
      FocusControl = txtParamValue
    end
    object btnOK: TButton
      Left = 221
      Top = 80
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TButton
      Left = 302
      Top = 80
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object chkSaveToFile: TCheckBox
      Left = 8
      Top = 55
      Width = 367
      Height = 21
      Caption = 'and save it as a custom parameter'
      TabOrder = 1
    end
    object txtParamValue: TEdit
      Left = 8
      Top = 24
      Width = 367
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
end
