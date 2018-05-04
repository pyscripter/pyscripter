inherited PythonVersionsDialog: TPythonVersionsDialog
  Caption = 'Python Versions'
  ClientHeight = 370
  ClientWidth = 630
  ExplicitWidth = 636
  ExplicitHeight = 399
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 630
    Height = 321
    Align = alTop
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 137
      Width = 628
      Height = 183
      Align = alClient
      TabOrder = 0
      ExplicitTop = 136
      ExplicitHeight = 192
      object gbNonRegisteredVersions: TGroupBox
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 620
        Height = 175
        Align = alClient
        Caption = 'Non-Registered Versions'
        TabOrder = 0
        ExplicitLeft = 8
        ExplicitTop = 12
        ExplicitHeight = 129
        object VirtualStringTree1: TVirtualStringTree
          Left = 2
          Top = 15
          Width = 616
          Height = 112
          Align = alTop
          Header.AutoSizeIndex = 0
          Header.MainColumn = -1
          TabOrder = 0
          ExplicitHeight = 114
          Columns = <>
        end
      end
    end
    object pnlNoRegisteredVersions: TPanel
      Left = 1
      Top = 1
      Width = 628
      Height = 136
      Align = alTop
      Caption = 'No registered python versions'
      TabOrder = 1
      object gbRegisteredVersions: TGroupBox
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 620
        Height = 128
        Align = alClient
        Caption = 'Registered Versions'
        TabOrder = 0
        ExplicitTop = -88
        ExplicitHeight = 129
        object vstRegisteredVersions: TVirtualStringTree
          Left = 2
          Top = 15
          Width = 616
          Height = 111
          Align = alClient
          Header.AutoSizeIndex = 0
          Header.MainColumn = -1
          TabOrder = 0
          ExplicitWidth = 620
          ExplicitHeight = 112
          Columns = <>
        end
      end
    end
  end
  object btnHelp: TButton
    Left = 517
    Top = 331
    Width = 94
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 1
    ExplicitTop = 338
  end
  object btnCancel: TButton
    Left = 413
    Top = 331
    Width = 94
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitTop = 338
  end
  object btnOk: TButton
    Left = 309
    Top = 331
    Width = 94
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    ExplicitTop = 338
  end
end
