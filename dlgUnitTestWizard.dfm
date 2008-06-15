inherited UnitTestWizard: TUnitTestWizard
  HelpContext = 930
  Caption = 'Unit Test Wizard'
  ClientHeight = 488
  ClientWidth = 436
  Font.Name = 'MS Shell Dlg 2'
  Position = poMainFormCenter
  ExplicitWidth = 442
  ExplicitHeight = 514
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 488
    ThemeType = thtWindows
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 2
    ExplicitTop = 4
    ExplicitWidth = 431
    DesignSize = (
      436
      488)
    object Bevel1: TBevel
      Left = 12
      Top = 8
      Width = 413
      Height = 73
      Shape = bsFrame
      Style = bsRaised
    end
    object ExplorerTree: TVirtualStringTree
      Left = 2
      Top = 106
      Width = 431
      Height = 335
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
      ExplicitWidth = 395
      Columns = <>
    end
    object Label1: TSpTBXLabel
      Left = 7
      Top = 87
      Width = 323
      Height = 13
      Caption = 
        'Select the functions and methods for which tests will be generat' +
        'ed:'
      ParentColor = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object lbHeader: TSpTBXLabel
      Left = 18
      Top = 15
      Width = 403
      Height = 60
      Caption = 'This wizard will generate unit tests for the Python module'
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentColor = True
      ParentFont = False
      Wrapping = twWrap
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
    object OKButton: TSpTBXButton
      Left = 99
      Top = 452
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 1
      ExplicitLeft = 63
      ExplicitTop = 413
    end
    object BitBtn2: TSpTBXButton
      Left = 195
      Top = 452
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Cancel = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
      ExplicitLeft = 159
      ExplicitTop = 413
    end
    object HelpButton: TSpTBXButton
      Left = 291
      Top = 452
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = HelpButtonClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ExplicitLeft = 255
      ExplicitTop = 413
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
