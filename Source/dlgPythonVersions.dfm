inherited PythonVersionsDialog: TPythonVersionsDialog
  HelpContext = 880
  Caption = 'Python Versions'
  ClientHeight = 315
  ClientWidth = 630
  ExplicitWidth = 636
  ExplicitHeight = 344
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 26
    Width = 630
    Height = 289
    Align = alClient
    TabOrder = 0
    object vtPythonVersions: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 628
      Height = 287
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Images = vilImages
      TabOrder = 0
      OnDblClick = actPVActivateExecute
      OnGetCellText = vtPythonVersionsGetCellText
      OnGetImageIndex = vtPythonVersionsGetImageIndex
      OnInitChildren = vtPythonVersionsInitChildren
      OnInitNode = vtPythonVersionsInitNode
      Columns = <
        item
          MinWidth = 250
          Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coStyleColor]
          Position = 0
          Text = 'Name'
          Width = 250
        end
        item
          MinWidth = 200
          Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coStyleColor]
          Position = 1
          Text = 'Folder'
          Width = 300
        end>
    end
  end
  object SpTBXDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 630
    Height = 26
    object SpTBXToolbar: TSpTBXToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DragHandleStyle = dhNone
      FullSize = True
      Images = vilImages
      Options = [tboShowHint, tboToolbarStyle]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      DisplayMode = tbdmImageOnly
      object tbiActivate: TSpTBXItem
        Action = actPVActivate
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object tbiPVAdd: TSpTBXItem
        Action = actPVAdd
      end
      object tbiPVRemove: TSpTBXItem
        Action = actPVRemove
      end
      object SpTBXItem1: TSpTBXItem
        Action = actPVRename
      end
      object TBSeparatorItem1: TTBSeparatorItem
      end
      object tbiPVTest: TSpTBXItem
        Action = actPVTest
      end
      object tbiPVShow: TSpTBXItem
        Action = actPVShow
      end
      object tbiPVCommandPrompt: TSpTBXItem
        Wrapping = twEndEllipsis
        Action = actPVCommandShell
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object tbiPVHelp: TSpTBXItem
        Action = actPVHelp
      end
    end
  end
  object actlPythonVersions: TActionList
    Images = vilImages
    OnUpdate = actlPythonVersionsUpdate
    Left = 560
    Top = 64
    object actPVActivate: TAction
      Caption = 'Activate'
      HelpContext = 880
      Hint = 'Activate selected Python version'
      ImageIndex = 7
      ImageName = 'Item160'
      OnExecute = actPVActivateExecute
    end
    object actPVAdd: TAction
      Caption = 'Add'
      HelpContext = 880
      Hint = 'Add a new Python version'
      ImageIndex = 1
      ImageName = 'Item50'
      OnExecute = actPVAddExecute
    end
    object actPVRemove: TAction
      Caption = 'Remove'
      HelpContext = 880
      Hint = 'Remove selected Python version'
      ImageIndex = 3
      ImageName = 'Item53'
      OnExecute = actPVRemoveExecute
    end
    object actPVTest: TAction
      Caption = 'Test'
      HelpContext = 880
      Hint = 'Test selected Python version'
      ImageIndex = 6
      ImageName = 'Item131'
      OnExecute = actPVTestExecute
    end
    object actPVShow: TAction
      Caption = 'Show'
      HelpContext = 880
      Hint = 'Show selected Python version in Explorer'
      ImageIndex = 0
      ImageName = 'Item3'
      OnExecute = actPVShowExecute
    end
    object actPVCommandShell: TAction
      Caption = 'Shell'
      HelpContext = 880
      Hint = 'Open command prompt for the selected Python version'
      ImageIndex = 5
      ImageName = 'Item90'
      OnExecute = actPVCommandShellExecute
    end
    object actPVHelp: TAction
      Caption = 'Help'
      HelpContext = 880
      Hint = 'Show Help'
      ImageIndex = 4
      ImageName = 'Item72'
      OnExecute = actPVHelpExecute
    end
    object actPVRename: TAction
      Caption = 'Rename'
      HelpContext = 880
      Hint = 'Rename selected Python version'
      ImageIndex = 8
      ImageName = 'Item161'
      OnExecute = actPVRenameExecute
    end
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Disabled = False
        Name = 'Item3'
      end
      item
        CollectionIndex = 49
        CollectionName = 'Item50'
        Disabled = False
        Name = 'Item50'
      end
      item
        CollectionIndex = 51
        CollectionName = 'Item52'
        Disabled = False
        Name = 'Item52'
      end
      item
        CollectionIndex = 52
        CollectionName = 'Item53'
        Disabled = False
        Name = 'Item53'
      end
      item
        CollectionIndex = 71
        CollectionName = 'Item72'
        Disabled = False
        Name = 'Item72'
      end
      item
        CollectionIndex = 89
        CollectionName = 'Item90'
        Disabled = False
        Name = 'Item90'
      end
      item
        CollectionIndex = 130
        CollectionName = 'Item131'
        Disabled = False
        Name = 'Item131'
      end
      item
        CollectionIndex = 159
        CollectionName = 'Item160'
        Disabled = False
        Name = 'Item160'
      end
      item
        CollectionIndex = 160
        CollectionName = 'Item161'
        Disabled = False
        Name = 'Item161'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 457
    Top = 65
  end
end
