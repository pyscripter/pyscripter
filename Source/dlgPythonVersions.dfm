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
    object gbPythonVersions: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 622
      Height = 281
      Align = alClient
      Caption = 'Python Versions'
      TabOrder = 0
      object vtPythonVersions: TVirtualStringTree
        Left = 2
        Top = 15
        Width = 618
        Height = 264
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Images = CommandsDataModule.Images
        TabOrder = 0
        OnGetCellText = vtPythonVersionsGetCellText
        OnGetImageIndex = vtPythonVersionsGetImageIndex
        OnInitChildren = vtPythonVersionsInitChildren
        OnInitNode = vtPythonVersionsInitNode
        Columns = <
          item
            MinWidth = 250
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coStyleColor]
            Position = 0
            Width = 250
            WideText = 'Name'
          end
          item
            MinWidth = 200
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coStyleColor]
            Position = 1
            Width = 300
            WideText = 'Folder'
          end>
      end
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
      Hint = 'Activate selected python version'
      ImageIndex = 159
      OnExecute = actPVActivateExecute
    end
    object actPVAdd: TAction
      Caption = 'Add'
      Hint = 'Add a new python version'
      ImageIndex = 49
      OnExecute = actPVAddExecute
    end
    object actPVRemove: TAction
      Caption = 'Remove'
      Hint = 'Remove selected python version'
      ImageIndex = 52
      OnExecute = actPVRemoveExecute
    end
    object actPVTest: TAction
      Caption = 'Test'
      Hint = 'Test selected python version'
      ImageIndex = 130
      OnExecute = actPVTestExecute
    end
    object actPVShow: TAction
      Caption = 'Show'
      Hint = 'Show selected python version in Explorer'
      ImageIndex = 2
      OnExecute = actPVShowExecute
    end
    object actPVCommandShell: TAction
      Caption = 'Shell'
      Hint = 'Open command prompt for the selected python version'
      ImageIndex = 89
      OnExecute = actPVCommandShellExecute
    end
    object actPVHelp: TAction
      Caption = 'Help'
      Hint = 'Show Help'
      ImageIndex = 71
      OnExecute = actPVHelpExecute
    end
  end
end
