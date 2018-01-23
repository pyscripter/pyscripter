inherited FileExplorerWindow: TFileExplorerWindow
  Left = 398
  Top = 201
  HelpContext = 420
  Caption = 'File Explorer'
  ClientHeight = 439
  Icon.Data = {
    0000010001001010000000000000680300001600000028000000100000002000
    0000010018000000000000030000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000005ACEFF5ACEFF5ACEFF0000000000000000
    000000000000000000000000000000000000000000000000000000006B6B6B00
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000005ACEFF5ACEFF5ACEFF0000000000000000
    000000000000000000000000000000000000000000000000000000006B6B6B00
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000ADFF0094DE0094DE0094DE0094DE
    0094DE0094DE0094DE0084BD0000000000000000000000000000000000000000
    005ACEFF00ADFF00ADFF00ADFF00ADFF00ADFF00ADFF00ADFF0094DE00000000
    00000000000000000000000000000000005ACEFF00ADFF00ADFF00ADFF00ADFF
    00ADFF00ADFF00ADFF0094DE0000000000000000000000000000000000000000
    005ACEFF00ADFF00ADFF00ADFF00ADFF00ADFF00ADFF00ADFF0094DE00000000
    00000000000000000000000000000000005ACEFF00ADFF00ADFF5ACEFF5ACEFF
    5ACEFF5ACEFF5ACEFF00ADFF0000000000000000000000000000000000006B6B
    6BBDEFFF5ACEFF5ACEFF00000000000000000000000000000000000000000000
    0000000000000000000000000000FFFFFF000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FBC1
    0000FD410000FBC10000FFFF0000FBC10000FD410000FBC10000FFFF0000001F
    0000001F0000001F0000001F0000001F0000001F0000003F00000FFF0000}
  ExplicitHeight = 478
  PixelsPerInch = 96
  TextHeight = 13
  inherited BGPanel: TSpTBXPanel
    Height = 439
    ExplicitHeight = 439
    inherited FGPanel: TPanel
      Height = 435
      ExplicitHeight = 435
      object FileExplorerTree: TVirtualExplorerTree
        Left = 0
        Top = 26
        Width = 223
        Height = 409
        Active = False
        Align = alClient
        AnimationDuration = 100
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ColumnDetails = cdUser
        ColumnMenuItemCount = 8
        DefaultNodeHeight = 17
        DragHeight = 250
        DragWidth = 150
        FileObjects = [foFolders, foNonFolders, foEnableAsync]
        FileSizeFormat = fsfExplorer
        FileSort = fsFileType
        Header.AutoSizeIndex = 0
        Header.Height = 17
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        HintMode = hmHint
        IncrementalSearch = isInitializedOnly
        ParentColor = False
        PopupMenu = ExplorerPopUp
        RootFolder = rfDrives
        ShellContextSubMenu = ShellContextPopUp
        ShellContextSubMenuCaption = 'File Explorer'
        TabOrder = 0
        TabStop = True
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowTreeLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toRightClickSelect]
        TreeOptions.VETFolderOptions = [toFoldersExpandable, toForceHideRecycleBin, toNoUseVETColorsProp, toThreadedExpandMark]
        TreeOptions.VETShellOptions = [toRightAlignSizeColumn, toContextMenus, toDragDrop, toShellHints]
        TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
        TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder, toChangeNotifierThread, toAutoScrollHorz, toRestoreTopNodeOnRefresh]
        TreeOptions.VETImageOptions = [toImages, toMarkCutAndCopy]
        OnDblClick = FileExplorerTreeDblClick
        OnEnumFolder = FileExplorerTreeEnumFolder
        OnKeyPress = FileExplorerTreeKeyPress
        Columns = <>
      end
      object ExplorerDock: TSpTBXDock
        Left = 0
        Top = 0
        Width = 223
        Height = 26
        AllowDrag = False
        object ExplorerToolbar: TSpTBXToolbar
          Left = 0
          Top = 0
          Align = alTop
          AutoResize = False
          DockMode = dmCannotFloat
          FullSize = True
          Images = CommandsDataModule.Images
          TabOrder = 0
          Caption = 'ExplorerToolbar'
          Customizable = False
          object tbiItemBack: TSpTBXSubmenuItem
            Action = actGoBack
            DropdownCombo = True
            OnPopup = tbiItemBackPopup
          end
          object tbiItemForward: TSpTBXSubmenuItem
            Action = actGoForward
            DropdownCombo = True
            OnPopup = tbiItemForwardPopup
          end
          object tbiGoUp: TSpTBXItem
            Action = actGoUp
          end
          object TBXSeparatorItem1: TSpTBXSeparatorItem
          end
          object TBXSubmenuItem1: TSpTBXSubmenuItem
            ImageIndex = 84
            Options = [tboDropdownArrow]
            object tbiBrowsePath: TSpTBXSubmenuItem
              Caption = 'Browse Path'
              LinkSubitems = mnBrowsePath
            end
            object mnFavorites: TSpTBXSubmenuItem
              Caption = '&Favorites'
              ImageIndex = 114
              OnPopup = mnFavoritesPopup
              object TBXSeparatorItem5: TSpTBXSeparatorItem
              end
              object TBXItem7: TSpTBXItem
                Caption = 'Add to &Favorites'
                Hint = 'Add current path to Favorites'
                HelpContext = 420
                OnClick = actAddToFavoritesExecute
              end
              object TBXItem2: TSpTBXItem
                Caption = '&Manage Favorites...'
                Hint = 'Manage favourite paths'
                HelpContext = 420
                OnClick = actManageFavoritesExecute
              end
            end
            object TBXSubmenuItem3: TSpTBXSubmenuItem
              Caption = 'Python Path'
              LinkSubitems = mnPythonPath
              OnPopup = BrowsePathPopup
            end
            object TBXSeparatorItem3: TSpTBXSeparatorItem
            end
            object tbiPythonPath: TSpTBXItem
              Caption = 'Manage Python &Path...'
              Action = CommandsDataModule.actPythonPath
            end
            object TBXSeparatorItem2: TSpTBXSeparatorItem
            end
            object tbiSearchPath: TSpTBXItem
              Action = actSearchPath
            end
          end
          object tbiEnableFilter: TSpTBXItem
            Action = actEnableFilter
          end
          object tbiNewFolder: TSpTBXItem
            Action = actNewFolder
          end
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 26
    Top = 38
  end
  object VirtualShellHistory: TVirtualShellHistory
    MenuOptions.Images = True
    OnChange = VirtualShellHistoryChange
    VirtualExplorerTree = FileExplorerTree
    Left = 34
    Top = 200
  end
  object ExplorerPopUp: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 30
    Top = 91
    object mnBack: TSpTBXItem
      Action = actGoBack
    end
    object mnForward: TSpTBXItem
      Action = actGoForward
    end
    object mnGoUp: TSpTBXItem
      Action = actGoUp
    end
    object N1: TSpTBXSeparatorItem
    end
    object mnBrowsePath: TSpTBXSubmenuItem
      Caption = 'Browse Path'
      object Desktop: TSpTBXItem
        Caption = 'Desktop'
        OnClick = DesktopClick
      end
      object MyComputer: TSpTBXItem
        Caption = 'My &Computer'
        OnClick = MyComputerClick
      end
      object MyDocuments: TSpTBXItem
        Caption = 'My &Documents'
        OnClick = MyDocumentsClick
      end
      object CurrentDirectory: TSpTBXItem
        Caption = 'Current Directory'
        OnClick = CurrentDirectoryClick
      end
      object ActiveScript: TSpTBXItem
        Caption = 'Active Script'
        OnClick = ActiveScriptClick
      end
    end
    object TBXSubmenuItem2: TSpTBXSubmenuItem
      Caption = 'Favorites'
      ImageIndex = 114
      LinkSubitems = mnFavorites
      OnPopup = mnFavoritesPopup
    end
    object mnPythonPath: TSpTBXSubmenuItem
      Caption = 'Python Path'
      OnPopup = BrowsePathPopup
    end
    object N2: TSpTBXSeparatorItem
    end
    object mnManagePythonPath: TSpTBXItem
      Caption = 'Manage Python &Path...'
      Action = CommandsDataModule.actPythonPath
    end
    object TBXSeparatorItem6: TSpTBXSeparatorItem
    end
    object mnEnableFilter: TSpTBXItem
      Action = actEnableFilter
    end
    object mnChangeFilter: TSpTBXItem
      Caption = '&Change Filter...'
      OnClick = mnChangeFilterClick
    end
    object N3: TSpTBXSeparatorItem
    end
    object mnNewFolder: TSpTBXItem
      Action = actNewFolder
    end
    object TBXSeparatorItem4: TSpTBXSeparatorItem
    end
    object mnRefresh: TSpTBXItem
      Action = actRefresh
    end
  end
  object ShellContextPopUp: TPopupMenu
    Images = CommandsDataModule.Images
    Left = 30
    Top = 148
    object ExploreHere: TMenuItem
      Caption = '&Explore Here'
      HelpContext = 420
      Hint = 'Set the Root of the File Manager to this folder'
      OnClick = ExploreHereClick
    end
    object AddToFavorites1: TMenuItem
      Action = actAddToFavorites
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object CreateNewFolder1: TMenuItem
      Action = actNewFolder
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object SearchPath1: TMenuItem
      Action = actSearchPath
    end
  end
  object FileExplorerActions: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = FileExplorerActionsUpdate
    Left = 35
    Top = 258
    object actGoBack: TAction
      Category = 'File Explorer'
      Caption = '&Back'
      HelpContext = 420
      Hint = 'Go Back'
      ImageIndex = 35
      OnExecute = actGoBackExecute
    end
    object actGoForward: TAction
      Category = 'File Explorer'
      Caption = '&Forward'
      HelpContext = 420
      Hint = 'Go Forward'
      ImageIndex = 36
      OnExecute = actGoForwardExecute
    end
    object actGoUp: TAction
      Category = 'File Explorer'
      Caption = '&Up'
      HelpContext = 420
      Hint = 'Up|Show the parent folder'
      ImageIndex = 45
      OnExecute = actGoUpExecute
    end
    object actRefresh: TAction
      Category = 'File Explorer'
      Caption = '&Refresh'
      HelpContext = 420
      Hint = 'Refresh|Refresh File Explorer'
      ImageIndex = 39
      ShortCut = 116
      OnExecute = actRefreshExecute
    end
    object actEnableFilter: TAction
      Category = 'File Explorer'
      AutoCheck = True
      Caption = 'Enable &Filter'
      Checked = True
      HelpContext = 420
      Hint = 'Enable Filter|Select to show only Python files'
      ImageIndex = 46
      OnExecute = actEnableFilterExecute
    end
    object actSearchPath: TAction
      Category = 'File Explorer'
      Caption = '&Search Path...'
      HelpContext = 420
      Hint = 'Search selected path'
      ImageIndex = 86
      OnExecute = actSearchPathExecute
    end
    object actExploreHere: TAction
      Category = 'File Explorer'
      Caption = '&Explore Here'
      HelpContext = 420
      Hint = 'Set the Root of the File Manager to this folder'
      OnExecute = ExploreHereClick
    end
    object actManageFavorites: TAction
      Category = 'File Explorer'
      Caption = '&Manage Favorites...'
      HelpContext = 420
      Hint = 'Manage favourite paths'
      OnExecute = actManageFavoritesExecute
    end
    object actAddToFavorites: TAction
      Category = 'File Explorer'
      Caption = 'Add to &Favorites'
      HelpContext = 420
      Hint = 'Add current path to Favorites'
      OnExecute = actAddToFavoritesExecute
    end
    object actNewFolder: TAction
      Category = 'File Explorer'
      Caption = 'Create &New Folder'
      HelpContext = 420
      Hint = 'Create new folder'
      ImageIndex = 115
      OnExecute = actNewFolderExecute
    end
  end
end
