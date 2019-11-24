inherited UnitTestWizard: TUnitTestWizard
  HelpContext = 930
  Caption = 'Unit Test Wizard'
  ClientHeight = 491
  ClientWidth = 436
  ExplicitWidth = 442
  ExplicitHeight = 520
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 491
    Align = alClient
    TabOrder = 0
    DesignSize = (
      436
      491)
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 428
      Height = 61
      Align = alTop
      Shape = bsFrame
      Style = bsRaised
      ExplicitLeft = 12
      ExplicitTop = 8
      ExplicitWidth = 413
    end
    object Label1: TLabel
      Left = 5
      Top = 81
      Width = 323
      Height = 13
      Caption = 
        'Select the functions and methods for which tests will be generat' +
        'ed:'
      Color = clNone
      ParentColor = False
    end
    object lbHeader: TLabel
      Left = 10
      Top = 15
      Width = 275
      Height = 13
      Caption = 'This wizard will generate unit tests for the Python module'
      Color = clNone
      ParentColor = False
    end
    object lbFileName: TLabel
      Left = 10
      Top = 36
      Width = 399
      Height = 13
      AutoSize = False
      Color = clNone
      ParentColor = False
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
      Header.AutoSizeIndex = -1
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintMode = hmHint
      Images = vilCodeImages
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
    object OKButton: TButton
      Left = 85
      Top = 454
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object BitBtn2: TButton
      Left = 181
      Top = 454
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object HelpButton: TButton
      Left = 277
      Top = 454
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
  end
  object PopupUnitTestWizard: TSpTBXPopupMenu
    Images = vilImages
    Left = 59
    Top = 118
    object mnSelectAll: TSpTBXItem
      Caption = 'Select All'
      Hint = 'Select all nodes'
      ImageIndex = 0
      OnClick = mnSelectAllClick
    end
    object mnDeselectAll: TSpTBXItem
      Caption = 'Deselect All'
      Hint = 'Deselect all nodes'
      ImageIndex = 1
      OnClick = mnDeselectAllClick
    end
  end
  object vilCodeImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'CodeImages\Item1'
        Disabled = False
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'CodeImages\Item2'
        Disabled = False
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'CodeImages\Item3'
        Disabled = False
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'CodeImages\Item4'
        Disabled = False
        Name = 'Item4'
      end
      item
        CollectionIndex = 4
        CollectionName = 'CodeImages\Item5'
        Disabled = False
        Name = 'Item5'
      end
      item
        CollectionIndex = 5
        CollectionName = 'CodeImages\Item6'
        Disabled = False
        Name = 'Item6'
      end
      item
        CollectionIndex = 6
        CollectionName = 'CodeImages\Item7'
        Disabled = False
        Name = 'Item7'
      end
      item
        CollectionIndex = 7
        CollectionName = 'CodeImages\Item8'
        Disabled = False
        Name = 'Item8'
      end
      item
        CollectionIndex = 8
        CollectionName = 'CodeImages\Item9'
        Disabled = False
        Name = 'Item9'
      end
      item
        CollectionIndex = 9
        CollectionName = 'CodeImages\Item10'
        Disabled = False
        Name = 'Item10'
      end>
    ImageCollection = CommandsDataModule.icCodeImages
    Left = 56
    Top = 184
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 104
        CollectionName = 'Item105'
        Disabled = False
        Name = 'Item105'
      end
      item
        CollectionIndex = 105
        CollectionName = 'Item106'
        Disabled = False
        Name = 'Item106'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 128
    Top = 184
  end
end
