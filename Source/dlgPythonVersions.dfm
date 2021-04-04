inherited PythonVersionsDialog: TPythonVersionsDialog
  HelpContext = 880
  Caption = 'Python Versions'
  ClientHeight = 315
  ClientWidth = 630
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
      ImageIndex = 3
      ImageName = 'PySetupRemove'
      OnExecute = actPVRemoveExecute
    end
    object actPVTest: TAction
      Caption = 'Test'
      HelpContext = 880
      Hint = 'Test selected Python version'
      ImageIndex = 6
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
      ImageIndex = 5
      ImageName = 'CmdOuputWin'
      OnExecute = actPVCommandShellExecute
    end
    object actPVHelp: TAction
      Caption = 'Help'
      HelpContext = 880
      Hint = 'Show Help'
      ImageIndex = 4
      ImageName = 'Help'
      OnExecute = actPVHelpExecute
    end
    object actPVRename: TAction
      Caption = 'Rename'
      HelpContext = 880
      Hint = 'Rename selected Python version'
      ImageIndex = 8
      ImageName = 'Rename'
      OnExecute = actPVRenameExecute
    end
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 45
        CollectionName = 'OpenFolder'
        Disabled = False
        Name = 'OpenFolder'
      end
      item
        CollectionIndex = 155
        CollectionName = 'PySetupAdd'
        Disabled = False
        Name = 'PySetupAdd'
      end
      item
        CollectionIndex = 51
        CollectionName = 'Run'
        Disabled = False
        Name = 'Run'
      end
      item
        CollectionIndex = 156
        CollectionName = 'PySetupRemove'
        Disabled = False
        Name = 'PySetupRemove'
      end
      item
        CollectionIndex = 33
        CollectionName = 'Help'
        Disabled = False
        Name = 'Help'
      end
      item
        CollectionIndex = 89
        CollectionName = 'CmdOuputWin'
        Disabled = False
        Name = 'CmdOuputWin'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ExternalRun'
        Disabled = False
        Name = 'ExternalRun'
      end
      item
        CollectionIndex = 159
        CollectionName = 'PyActivate'
        Disabled = False
        Name = 'PyActivate'
      end
      item
        CollectionIndex = 160
        CollectionName = 'Rename'
        Disabled = False
        Name = 'Rename'
      end>
    ImageCollection = CommandsDataModule.icImages
    PreserveItems = True
    Left = 457
    Top = 65
  end
end
