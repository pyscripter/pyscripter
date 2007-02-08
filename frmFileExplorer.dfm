inherited FileExplorerWindow: TFileExplorerWindow
  Left = 398
  Top = 201
  HelpKeyword = 'The File Explorer'
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
  OnActivate = FormActivate
  ExplicitHeight = 465
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Left = 6
    Top = 7
    Width = 229
    Height = 415
    ExplicitLeft = 6
    ExplicitTop = 7
    ExplicitWidth = 229
    ExplicitHeight = 415
    object FileExplorerTree: TVirtualExplorerTree
      Left = 0
      Top = 26
      Width = 229
      Height = 389
      Active = False
      Align = alClient
      AnimationDuration = 100
      BevelInner = bvLowered
      BevelOuter = bvNone
      BevelKind = bkSoft
      BorderStyle = bsNone
      ColumnDetails = cdUser
      ColumnMenuItemCount = 8
      DefaultNodeHeight = 17
      DragHeight = 250
      DragWidth = 150
      FileObjects = [foFolders, foNonFolders]
      FileSizeFormat = fsfExplorer
      FileSort = fsFileType
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
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
      TreeOptions.PaintOptions = [toShowButtons, toShowTreeLines, toUseBlendedImages, toGhostedIfUnfocused, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toRightClickSelect]
      TreeOptions.VETFolderOptions = [toFoldersExpandable, toForceHideRecycleBin]
      TreeOptions.VETShellOptions = [toRightAlignSizeColumn, toContextMenus, toShellHints]
      TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
      TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder, toAutoScrollHorz, toRestoreTopNodeOnRefresh]
      TreeOptions.VETImageOptions = [toImages, toMarkCutAndCopy]
      OnDblClick = FileExplorerTreeDblClick
      OnEnumFolder = FileExplorerTreeEnumFolder
      OnKeyPress = FileExplorerTreeKeyPress
      Columns = <>
    end
    object ExplorerDock: TTBXDock
      Left = 0
      Top = 0
      Width = 229
      Height = 26
      AllowDrag = False
      object ExplorerToolbar: TTBXToolbar
        Left = 0
        Top = 0
        Align = alTop
        AutoResize = False
        Caption = 'ExplorerToolbar'
        DockMode = dmCannotFloat
        FullSize = True
        Images = CommandsDataModule.Images
        TabOrder = 0
        object TBXItemBack: TTBXSubmenuItem
          Action = actGoBack
          DropdownCombo = True
          OnPopup = TBXItemBackPopup
        end
        object TBXItemForward: TTBXSubmenuItem
          Action = actGoForward
          DropdownCombo = True
          OnPopup = TBXItemForwardPopup
        end
        object TBXItem3: TTBXItem
          Action = actGoUp
        end
        object TBXSeparatorItem1: TTBXSeparatorItem
        end
        object TBXSubmenuItem1: TTBXSubmenuItem
          ImageIndex = 84
          Options = [tboDropdownArrow]
          object TBSubmenuItem1: TTBXSubmenuItem
            Caption = 'Browse Path'
            LinkSubitems = BrowsePath
          end
          object mnFavourites: TTBXSubmenuItem
            Caption = '&Favourites'
            ImageIndex = 114
            OnPopup = mnFavouritesPopup
            object TBXSeparatorItem5: TTBXSeparatorItem
            end
            object TBXItem7: TTBXItem
              Action = actAddToFavourites
            end
            object TBXItem2: TTBXItem
              Action = actManageFavourites
            end
          end
          object TBXSubmenuItem3: TTBXSubmenuItem
            Caption = 'Python Path'
            LinkSubitems = TBXPythonPath
            OnPopup = BrowsePathPopup
          end
          object TBXSeparatorItem3: TTBXSeparatorItem
          end
          object TBXItem6: TTBXItem
            Action = CommandsDataModule.actPythonPath
            Caption = 'Manage Python &Path...'
          end
          object TBXSeparatorItem2: TTBXSeparatorItem
          end
          object TBXItem1: TTBXItem
            Action = actSearchPath
          end
        end
        object TBXItem5: TTBXItem
          Action = actEnableFilter
        end
        object TBXItem10: TTBXItem
          Action = actNewFolder
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 186
    Top = 206
  end
  object VirtualShellHistory: TVirtualShellHistory
    MenuOptions.Images = True
    OnChange = VirtualShellHistoryChange
    VirtualExplorerTree = FileExplorerTree
    Left = 186
    Top = 304
  end
  object FileExplorerActions: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = FileExplorerActionsUpdate
    Left = 187
    Top = 334
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
    object actManageFavourites: TAction
      Category = 'File Explorer'
      Caption = '&Manage Favourites...'
      HelpContext = 420
      Hint = 'Manage favourite paths'
      OnExecute = actManageFavouritesExecute
    end
    object actAddToFavourites: TAction
      Category = 'File Explorer'
      Caption = 'Add to &Favourites'
      HelpContext = 420
      Hint = 'Add current path to Favourites'
      OnExecute = actAddToFavouritesExecute
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
  object ExplorerPopUp: TTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 186
    Top = 239
    object Back1: TTBXItem
      Action = actGoBack
    end
    object About1: TTBXItem
      Action = actGoForward
    end
    object Up1: TTBXItem
      Action = actGoUp
    end
    object N1: TTBXSeparatorItem
    end
    object BrowsePath: TTBXSubmenuItem
      Caption = 'Browse Path'
      object Desktop: TTBXItem
        Caption = 'Desktop'
        OnClick = DesktopClick
      end
      object MyComputer: TTBXItem
        Caption = 'My &Computer'
        OnClick = MyComputerClick
      end
      object MyDocuments: TTBXItem
        Caption = 'My &Documents'
        OnClick = MyDocumentsClick
      end
      object CurrentDirectory: TTBXItem
        Caption = 'Current Directory'
        OnClick = CurrentDirectoryClick
      end
      object ActiveScript: TTBXItem
        Caption = 'Active Script'
        OnClick = ActiveScriptClick
      end
    end
    object TBXSubmenuItem2: TTBXSubmenuItem
      Caption = 'Favourites'
      ImageIndex = 114
      LinkSubitems = mnFavourites
      OnPopup = mnFavouritesPopup
    end
    object TBXPythonPath: TTBXSubmenuItem
      Caption = 'Python Path'
      OnPopup = BrowsePathPopup
    end
    object N2: TTBXSeparatorItem
    end
    object PythonPath1: TTBXItem
      Action = CommandsDataModule.actPythonPath
      Caption = 'Manage Python &Path...'
    end
    object TBXSeparatorItem6: TTBXSeparatorItem
    end
    object EnableFilter: TTBXItem
      Action = actEnableFilter
    end
    object ChangeFilter: TTBXItem
      Caption = '&Change Filter...'
      OnClick = ChangeFilterClick
    end
    object N3: TTBXSeparatorItem
    end
    object TBXItem8: TTBXItem
      Action = actNewFolder
    end
    object TBXSeparatorItem4: TTBXSeparatorItem
    end
    object Refresh1: TTBXItem
      Action = actRefresh
    end
  end
  object ShellContextPopUp: TPopupMenu
    Images = CommandsDataModule.Images
    Left = 186
    Top = 272
    object ExploreHere: TMenuItem
      Action = actExploreHere
    end
    object AddToFavourites1: TMenuItem
      Action = actAddToFavourites
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
end
