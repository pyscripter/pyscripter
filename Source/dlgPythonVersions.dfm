inherited PythonVersionsDialog: TPythonVersionsDialog
  HelpContext = 880
  BorderStyle = bsSizeToolWin
  Caption = 'Python Versions'
  ClientHeight = 315
  ClientWidth = 630
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 30
    Width = 630
    Height = 285
    Align = alClient
    TabOrder = 0
    object vtPythonVersions: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 628
      Height = 283
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Images = vilTreeImages
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
      OnDblClick = actPVActivateExecute
      OnGetCellText = vtPythonVersionsGetCellText
      OnGetImageIndex = vtPythonVersionsGetImageIndex
      OnInitChildren = vtPythonVersionsInitChildren
      OnInitNode = vtPythonVersionsInitNode
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
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
    Height = 30
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
      object SpTBXSeparatorItem3: TSpTBXSeparatorItem
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
      ImageIndex = 6
      ImageName = 'PyActivate'
      OnExecute = actPVActivateExecute
    end
    object actPVAdd: TAction
      Caption = 'Add'
      HelpContext = 880
      Hint = 'Add a new Python version'
      ImageIndex = 1
      ImageName = 'PySetupAdd'
      OnExecute = actPVAddExecute
    end
    object actPVRemove: TAction
      Caption = 'Remove'
      HelpContext = 880
      Hint = 'Remove selected Python version'
      ImageIndex = 2
      ImageName = 'PySetupRemove'
      OnExecute = actPVRemoveExecute
    end
    object actPVTest: TAction
      Caption = 'Test'
      HelpContext = 880
      Hint = 'Test selected Python version'
      ImageIndex = 5
      ImageName = 'ExternalRun'
      OnExecute = actPVTestExecute
    end
    object actPVShow: TAction
      Caption = 'Show'
      HelpContext = 880
      Hint = 'Show selected Python version in Explorer'
      ImageIndex = 0
      ImageName = 'OpenFolder'
      OnExecute = actPVShowExecute
    end
    object actPVCommandShell: TAction
      Caption = 'Shell'
      HelpContext = 880
      Hint = 'Open command prompt for the selected Python version'
      ImageIndex = 4
      ImageName = 'CmdOuputWin'
      OnExecute = actPVCommandShellExecute
    end
    object actPVHelp: TAction
      Caption = 'Help'
      HelpContext = 880
      Hint = 'Show Help'
      ImageIndex = 3
      ImageName = 'Help'
      OnExecute = actPVHelpExecute
    end
    object actPVRename: TAction
      Caption = 'Rename'
      HelpContext = 880
      Hint = 'Rename selected Python version'
      ImageIndex = 7
      ImageName = 'Rename'
      OnExecute = actPVRenameExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 63
        CollectionName = 'OpenFolder'
        Name = 'OpenFolder'
      end
      item
        CollectionIndex = 82
        CollectionName = 'PySetupAdd'
        Name = 'PySetupAdd'
      end
      item
        CollectionIndex = 83
        CollectionName = 'PySetupRemove'
        Name = 'PySetupRemove'
      end
      item
        CollectionIndex = 52
        CollectionName = 'Help'
        Name = 'Help'
      end
      item
        CollectionIndex = 12
        CollectionName = 'CmdOuputWin'
        Name = 'CmdOuputWin'
      end
      item
        CollectionIndex = 32
        CollectionName = 'ExternalRun'
        Name = 'ExternalRun'
      end
      item
        CollectionIndex = 79
        CollectionName = 'PyActivate'
        Name = 'PyActivate'
      end
      item
        CollectionIndex = 90
        CollectionName = 'Rename'
        Name = 'Rename'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 457
    Top = 65
  end
  object vilTreeImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 3
        CollectionName = 'ArrowRight'
        Name = 'ArrowRight'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    Left = 456
    Top = 120
  end
end
