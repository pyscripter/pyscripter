inherited UnitTestWizard: TUnitTestWizard
  HelpContext = 930
  Caption = 'Unit Test Wizard'
  ClientHeight = 491
  ClientWidth = 436
  ExplicitWidth = 442
  ExplicitHeight = 519
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 491
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      436
      491)
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 426
      Height = 61
      Align = alTop
      Shape = bsFrame
      Style = bsRaised
      ExplicitLeft = 12
      ExplicitTop = 8
      ExplicitWidth = 413
    end
    object ExplorerTree: TVirtualStringTree
      Left = 5
      Top = 106
      Width = 426
      Height = 338
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelKind = bkSoft
      BorderStyle = bsNone
      BorderWidth = 2
      CheckImageKind = ckXP
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintMode = hmHint
      Images = CommandsDataModule.CodeImages
      IncrementalSearch = isAll
      ParentShowHint = False
      PopupMenu = PopupUnitTestWizard
      ShowHint = True
      TabOrder = 0
      TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnGetText = ExplorerTreeGetText
      OnGetImageIndex = ExplorerTreeGetImageIndex
      OnGetHint = ExplorerTreeGetHint
      OnInitChildren = ExplorerTreeInitChildren
      OnInitNode = ExplorerTreeInitNode
      Columns = <>
    end
    object Label1: TSpTBXLabel
      Left = 5
      Top = 81
      Width = 329
      Height = 19
      Caption = 
        'Select the functions and methods for which tests will be generat' +
        'ed:'
    end
    object lbHeader: TSpTBXLabel
      Left = 10
      Top = 15
      Width = 281
      Height = 19
      Caption = 'This wizard will generate unit tests for the Python module'
    end
    object OKButton: TSpTBXButton
      Left = 85
      Top = 454
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      ModalResult = 1
    end
    object BitBtn2: TSpTBXButton
      Left = 181
      Top = 454
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Cancel = True
      ModalResult = 2
    end
    object HelpButton: TSpTBXButton
      Left = 277
      Top = 454
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = HelpButtonClick
    end
    object lbFileName: TSpTBXLabel
      Left = 10
      Top = 36
      Width = 417
      Height = 19
      AutoSize = False
      Wrapping = twPathEllipsis
    end
  end
  object PopupUnitTestWizard: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 152
    Top = 112
    object mnSelectAll: TSpTBXItem
      Caption = 'Select All'
      Hint = 'Select all nodes'
      ImageIndex = 104
      OnClick = mnSelectAllClick
    end
    object mnDeselectAll: TSpTBXItem
      Caption = 'Deselect All'
      Hint = 'Deselect all nodes'
      ImageIndex = 105
      OnClick = mnDeselectAllClick
    end
  end
end
