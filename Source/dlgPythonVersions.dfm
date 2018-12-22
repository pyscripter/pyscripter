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
      DefaultText = 'Node'
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Images = CommandsDataModule.Images
      TabOrder = 0
      OnDblClick = actPVActivateExecute
      OnGetCellText = vtPythonVersionsGetCellText
      OnGetImageIndex = vtPythonVersionsGetImageIndex
      OnInitChildren = vtPythonVersionsInitChildren
      OnInitNode = vtPythonVersionsInitNode
      ExplicitLeft = 2
      ExplicitTop = 15
      ExplicitWidth = 618
      ExplicitHeight = 264
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
      Images = CommandsDataModule.Images
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
    Images = CommandsDataModule.Images
    OnUpdate = actlPythonVersionsUpdate
    Left = 560
    Top = 64
    object actPVActivate: TAction
      Caption = 'Activate'
      HelpContext = 880
      Hint = 'Activate selected Python version'
      ImageIndex = 159
      OnExecute = actPVActivateExecute
    end
    object actPVAdd: TAction
      Caption = 'Add'
      HelpContext = 880
      Hint = 'Add a new Python version'
      ImageIndex = 49
      OnExecute = actPVAddExecute
    end
    object actPVRemove: TAction
      Caption = 'Remove'
      HelpContext = 880
      Hint = 'Remove selected Python version'
      ImageIndex = 52
      OnExecute = actPVRemoveExecute
    end
    object actPVTest: TAction
      Caption = 'Test'
      HelpContext = 880
      Hint = 'Test selected Python version'
      ImageIndex = 130
      OnExecute = actPVTestExecute
    end
    object actPVShow: TAction
      Caption = 'Show'
      HelpContext = 880
      Hint = 'Show selected Python version in Explorer'
      ImageIndex = 2
      OnExecute = actPVShowExecute
    end
    object actPVCommandShell: TAction
      Caption = 'Shell'
      HelpContext = 880
      Hint = 'Open command prompt for the selected Python version'
      ImageIndex = 89
      OnExecute = actPVCommandShellExecute
    end
    object actPVHelp: TAction
      Caption = 'Help'
      HelpContext = 880
      Hint = 'Show Help'
      ImageIndex = 71
      OnExecute = actPVHelpExecute
    end
    object actPVRename: TAction
      Caption = 'Rename'
      HelpContext = 880
      Hint = 'Rename selected Python version'
      ImageIndex = 160
      OnExecute = actPVRenameExecute
    end
  end
end
