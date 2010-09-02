inherited ConfirmReplaceDialog: TConfirmReplaceDialog
  Left = 176
  Top = 158
  Caption = 'Confirm replace'
  ClientHeight = 98
  ClientWidth = 328
  Position = poDefaultSizeOnly
  OnDestroy = FormDestroy
  ExplicitWidth = 334
  ExplicitHeight = 126
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 98
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    object Image1: TImage
      Left = 16
      Top = 16
      Width = 32
      Height = 32
    end
    object btnReplace: TSpTBXButton
      Left = 8
      Top = 67
      Width = 75
      Height = 23
      Caption = '&Yes'
      TabOrder = 0
      Default = True
      ModalResult = 6
    end
    object btnSkip: TSpTBXButton
      Left = 87
      Top = 67
      Width = 75
      Height = 23
      Caption = '&No'
      TabOrder = 1
      ModalResult = 7
    end
    object btnCancel: TSpTBXButton
      Left = 166
      Top = 67
      Width = 75
      Height = 23
      Caption = '&Cancel'
      TabOrder = 2
      Cancel = True
      ModalResult = 2
    end
    object btnReplaceAll: TSpTBXButton
      Left = 245
      Top = 67
      Width = 75
      Height = 23
      Caption = 'Yes to &all'
      TabOrder = 3
      ModalResult = 10
    end
    object lblConfirmation: TSpTBXLabel
      Left = 60
      Top = 12
      Width = 261
      Height = 44
      AutoSize = False
      Wrapping = twWrap
    end
  end
end
