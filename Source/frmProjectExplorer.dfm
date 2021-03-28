inherited ProjectExplorerWindow: TProjectExplorerWindow
  HelpContext = 435
  Caption = 'Project Explorer'
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000000000
    00000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF3083
    AFFF3083AFFF3083AFFF3083AFFFFFFFFFFFFFFFFFFF00000000000000000000
    000000000000000000000000000000000000FFFFFFFF3083AFFF3083AFFFE1F3
    F9FFD4F7FFFF78BAE1FF78BAE1FF3083AFFF3083AFFFFFFFFFFF000000000000
    0000000000000000000000000000FFFFFFFF63A5C6FFF2FBFEFFFFFFFFFFB3EC
    FFFFC6EBFFFF73B6D6FF93D3F5FF93D3F5FF78BAE1FF3083AFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF63A5C6FFF5FFFFFFA2DDFAFFA7E0
    FBFFBFE7FBFF70B1D3FF81C6EAFF91CDEDFF93D3F5FF4A99C1FFFFFFFFFFFFFF
    FFFFD7BC9BFFD9B38CFFD9AD81FFD9AD81FF63A5C6FFF0FFFFFFB9E5F8FFC0E7
    FBFFF1FBFDFF78BAE1FF81C6EAFF91D2F2FF93D3F5FF4697C0FFFFFFFFFFFFFF
    FFFFD9B38CFFF5DABEFFF5DABEFFF5DABEFF63A5C6FFFFFFFFFFD7EEF9FFEEFB
    FFFFB8E4FAFFB8E4FAFF78BAE1FF78BAE1FF81C6EAFF4D9CC5FFFFFFFFFFFFFF
    FFFFD9B38CFFFFF4EAFFD9A066FFFFF4EAFF63A5C6FFFFFFFFFFB8E4FAFFB8E4
    FAFFB8E4FAFF9AD0ECFFB8E4FAFFB8E4FAFF81C6EAFF4E9FC9FFFFFFFFFFFFFF
    FFFFD7BC9BFFF5DABEFFF5DABEFFF5DABEFF63A5C6FFB8E4FAFFB8E4FAFF9AD0
    ECFF8FC9E7FF8AC4E1FF9AD0ECFFB8E4FAFFB8E4FAFF4389ABFFFFFFFFFFFFFF
    FFFFD7BC9BFFFFF4EAFFD9A066FFFFF4EAFFFFF4EAFF63A5C6FF63A5C6FFB8E4
    FAFF9AD0ECFFB8E4FAFFB8E4FAFF63A5C6FF63A5C6FFFFFFFFFF00000000FFFF
    FFFFCBB797FFF5DABEFFF5DABEFFF5DABEFFF5DABEFFF5DABEFFF5DABEFF63A5
    C6FF63A5C6FF63A5C6FF63A5C6FFFFFFFFFFFFFFFFFF0000000000000000FFFF
    FFFFCBB797FFFFF4EAFFD9A066FFFFF4EAFFFFF4EAFFFFF4EAFFFFF4EAFFFFF4
    EAFFFFF4EAFFFFF4EAFFD9AD81FFFFFFFFFF000000000000000000000000FFFF
    FFFFD7BC9BFFF5DABEFFF5DABEFFF5DABEFFF5DABEFFF5DABEFFF5DABEFFF5DA
    BEFFF5DABEFFF5DABEFFD9B38CFFFFFFFFFF000000000000000000000000FFFF
    FFFFD7BC9BFFFFF4EAFFFFF4EAFFFFF4EAFFFFF4EAFFFFF4EAFFFFF4EAFFFFF4
    EAFFFFF4EAFFFFF4EAFFD9AD81FFFFFFFFFF000000000000000000000000FFFF
    FFFFB99154FFB9823EFFB9823EFFB9773DFFB9773DFFB9773DFFB9773DFFB977
    3DFFB9773DFFB9773DFFB9773DFFFFFFFFFF000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FF0F
    9C41FC039C41F8019C41F0009C4100009C4100009C4100009C4100009C410000
    9C4100019C4100039C4100079C4100079C4100079C4100079C4100079C41}
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited BGPanel: TPanel
    inherited FGPanel: TPanel
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 223
        Height = 404
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ExplorerTree: TVirtualStringTree
          Left = 0
          Top = 26
          Width = 223
          Height = 378
          Align = alClient
          BorderStyle = bsNone
          Header.AutoSizeIndex = -1
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag]
          HintMode = hmHintAndDefault
          Images = vilProjects
          IncrementalSearch = isAll
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoChangeScale]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
          TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect, toSiblingSelectConstraint]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnContextPopup = ExplorerTreeContextPopup
          OnDragAllowed = ExplorerTreeDragAllowed
          OnDragOver = ExplorerTreeDragOver
          OnDragDrop = ExplorerTreeDragDrop
          OnEditing = ExplorerTreeEditing
          OnGetCellText = ExplorerTreeGetCellText
          OnGetImageIndexEx = ExplorerTreeGetImageIndexEx
          OnGetHint = ExplorerTreeGetHint
          OnIncrementalSearch = ExplorerTreeIncrementalSearch
          OnInitChildren = ExplorerTreeInitChildren
          OnInitNode = ExplorerTreeInitNode
          OnKeyPress = ExplorerTreeKeyPress
          OnNewText = ExplorerTreeNewText
          OnNodeDblClick = ExplorerTreeNodeDblClick
          Columns = <>
        end
        object SpTBXDock1: TSpTBXDock
          Left = 0
          Top = 0
          Width = 223
          Height = 26
          AllowDrag = False
          DoubleBuffered = True
          object SpTBXToolbar1: TSpTBXToolbar
            Left = 0
            Top = 0
            Align = alTop
            AutoResize = False
            DockMode = dmCannotFloat
            FullSize = True
            Images = vilImages
            TabOrder = 0
            Caption = 'Project Explorer Toolbar'
            Customizable = False
            object tbiProjectNew: TSpTBXItem
              Action = actProjectNew
            end
            object tbiProjectOpen: TSpTBXItem
              Action = actProjectOpen
            end
            object tbiProjectSave: TSpTBXItem
              Action = actProjectSave
            end
            object SpTBXSeparatorItem8: TSpTBXSeparatorItem
            end
            object tbiRunLast: TSpTBXItem
              Action = PyIDEMainForm.actRunLastScript
              ImageIndex = 17
            end
            object tbiDebugLast: TSpTBXItem
              Action = PyIDEMainForm.actRunDebugLastScript
              ImageIndex = 18
            end
            object tbiRunLastExternal: TSpTBXItem
              Action = PyIDEMainForm.actRunLastScriptExternal
              ImageIndex = 19
            end
            object SpTBXSeparatorItem11: TSpTBXSeparatorItem
            end
            object tbiExpandAll: TSpTBXItem
              Action = actProjectExpandAll
            end
            object tbiCollapseAll: TSpTBXItem
              Action = actProjectCollapseAll
            end
          end
        end
      end
    end
  end
  object ProjectActionList: TActionList [1]
    Images = vilImages
    Left = 41
    Top = 76
    object actProjectNew: TAction
      Category = 'Project'
      Caption = '&New Project'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Start a new project'
      ImageIndex = 6
      ImageName = 'Item134'
      OnExecute = actProjectNewExecute
    end
    object actProjectOpen: TAction
      Category = 'Project'
      Caption = '&Open Project...'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Open a project file'
      ImageIndex = 7
      ImageName = 'Item135'
      OnExecute = actProjectOpenExecute
    end
    object actProjectSave: TAction
      Category = 'Project'
      Caption = '&Save Project'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Save the project'
      ImageIndex = 12
      ImageName = 'Item141'
      OnExecute = actProjectSaveExecute
    end
    object actProjectSaveAs: TAction
      Category = 'Project'
      Caption = 'Save Project &As...'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Save project with under a different name'
      OnExecute = actProjectSaveAsExecute
    end
    object actProjectRelativePaths: TAction
      Category = 'Project'
      AutoCheck = True
      Caption = 'Store &Relative Paths'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Store file paths relative to the project directory'
      OnExecute = actProjectRelativePathsExecute
    end
    object actProjectShowFileExtensions: TAction
      Category = 'Project'
      AutoCheck = True
      Caption = 'Show File &Extensions'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Display file extensions'
      OnExecute = actProjectShowFileExtensionsExecute
    end
    object actProjectExtraPythonPath: TAction
      Category = 'Project'
      Caption = 'Extra Python &Path...'
      HelpKeyword = '435'
      HelpType = htContext
      ImageIndex = 0
      ImageName = 'Item26'
      OnExecute = actProjectExtraPythonPathExecute
    end
    object actProjectExpandAll: TAction
      Category = 'Project'
      Caption = '&Expand All'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Expand all project nodes'
      ImageIndex = 2
      ImageName = 'Item29'
      OnExecute = actProjectExpandAllExecute
    end
    object actProjectCollapseAll: TAction
      Category = 'Project'
      Caption = '&Collapse All'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Collapse all project nodes'
      ImageIndex = 3
      ImageName = 'Item30'
      OnExecute = actProjectCollapseAllExecute
    end
  end
  object ImmutableProjectActionList: TActionList [2]
    Images = vilImages
    Left = 8
    Top = 76
    object actProjectAddFiles: TAction
      Category = 'Project'
      Caption = '&Add File(s)...'
      HelpContext = 435
      Hint = 'Add file(s) to a project folder'
      ImageIndex = 8
      ImageName = 'Item136'
      OnExecute = actProjectAddFilesExecute
    end
    object actProjectAddFolder: TAction
      Category = 'Project'
      Caption = 'Add &Subfolder'
      HelpContext = 435
      Hint = 'Add a new subfolder'
      ImageIndex = 10
      ImageName = 'Item138'
      OnExecute = actProjectAddFolderExecute
    end
    object actProjectRemove: TAction
      Category = 'Project'
      Caption = '&Remove'
      HelpContext = 435
      Hint = 'Remove a file, folder or run configuration from the project'
      ImageIndex = 9
      ImageName = 'Item137'
      OnExecute = actProjectRemoveExecute
    end
    object actProjectRename: TAction
      Category = 'Project'
      Caption = 'Re&name'
      HelpContext = 435
      Hint = 'Rename the selected folder or RunConfiguration'
      OnExecute = actProjectRenameExecute
    end
    object actProjectFileEdit: TAction
      Category = 'Project'
      Caption = '&Edit'
      HelpContext = 435
      Hint = 'Open the selected file in the editor'
      ImageIndex = 11
      ImageName = 'Item140'
      OnExecute = actProjectFileEditExecute
    end
    object actProjectAddActiveFile: TAction
      Category = 'Project'
      Caption = 'Add Active File'
      HelpContext = 435
      Hint = 'Add the active editor file to the folder'
      OnExecute = actProjectAddActiveFileExecute
    end
    object actProjectImportDirectory: TAction
      Category = 'Project'
      Caption = '&Import Directory...'
      HelpContext = 435
      Hint = 'Import a directory into the selected folder'
      OnExecute = actProjectImportDirectoryExecute
    end
    object actProjectAddRunConfig: TAction
      Category = 'Project'
      Caption = 'Add Run Configuration'
      HelpContext = 435
      Hint = 'Add run configuration'
      ImageIndex = 13
      ImageName = 'Item142'
      OnExecute = actProjectAddRunConfigExecute
    end
    object actProjectEditRunConfig: TAction
      Category = 'Project'
      Caption = '&Edit Run Configuration'
      HelpContext = 435
      Hint = 'Edit run configuration'
      ImageIndex = 14
      ImageName = 'Item143'
      OnExecute = actProjectEditRunConfigExecute
    end
    object actProjectFileProperties: TAction
      Category = 'Project'
      Caption = '&Properties'
      HelpContext = 435
      Hint = 'Show file properties'
      ImageIndex = 15
      ImageName = 'Item144'
      OnExecute = actProjectFilePropertiesExecute
    end
    object actProjectRun: TAction
      Category = 'Project'
      Caption = '&Run'
      HelpContext = 435
      Hint = 'Run the selected configuration'
      ImageIndex = 4
      ImageName = 'Item52'
      OnExecute = actProjectRunExecute
    end
    object actProjectExternalRun: TAction
      Category = 'Project'
      Caption = 'E&xternal Run'
      HelpContext = 435
      Hint = 'Run configuration using an external Python interpreter'
      ImageIndex = 1
      ImageName = 'Item28'
      OnExecute = actProjectExternalRunExecute
    end
    object actProjectDebug: TAction
      Category = 'Project'
      Caption = '&Debug'
      HelpContext = 435
      Hint = 'Debug the selected configuration'
      ImageIndex = 5
      ImageName = 'Item54'
      OnExecute = actProjectDebugExecute
    end
    object actProjectAddRemoteFile: TAction
      Category = 'Project'
      Caption = 'Add Remote File'
      HelpContext = 435
      Hint = 'Add a remote file to the folder'
      ImageIndex = 16
      ImageName = 'Item162'
      OnExecute = actProjectAddRemoteFileExecute
    end
  end
  inherited DockClient: TJvDockClient
    Top = 46
  end
  object ProjectMainPopUpMenu: TSpTBXPopupMenu
    Images = vilImages
    OnPopup = ProjectMainPopUpMenuPopup
    Left = 8
    Top = 108
    object mnProjectNew: TSpTBXItem
      Action = actProjectNew
    end
    object mnProjectOpen: TSpTBXItem
      Action = actProjectOpen
    end
    object mnProjectSave: TSpTBXItem
      Action = actProjectSave
    end
    object mnProjectSaveAs: TSpTBXItem
      Action = actProjectSaveAs
    end
    object SpTBXSeparatorItem10: TSpTBXSeparatorItem
    end
    object mnExpandAll: TSpTBXItem
      Action = actProjectExpandAll
    end
    object mnCollapseAll: TSpTBXItem
      Action = actProjectCollapseAll
    end
    object SpTBXSeparatorItem9: TSpTBXSeparatorItem
    end
    object mnShowFileExt: TSpTBXItem
      Action = actProjectShowFileExtensions
    end
    object mnStoreRelativePaths: TSpTBXItem
      Action = actProjectRelativePaths
    end
    object SpTBXSeparatorItem12: TSpTBXSeparatorItem
    end
    object mnExtraPythonPath: TSpTBXItem
      Action = actProjectExtraPythonPath
    end
  end
  object ProjectFolderPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 40
    Top = 109
    object mnAddFiles: TSpTBXItem
      Action = actProjectAddFiles
    end
    object mnAddActiveFile: TSpTBXItem
      Action = actProjectAddActiveFile
    end
    object SpTBXItem1: TSpTBXItem
      Action = actProjectAddRemoteFile
    end
    object SpTBXItem6: TSpTBXItem
      Action = actProjectAddFolder
    end
    object mnImportDir: TSpTBXItem
      Action = actProjectImportDirectory
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnRename: TSpTBXItem
      Action = actProjectRename
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnRemove: TSpTBXItem
      Action = actProjectRemove
    end
  end
  object ProjectFilePopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 72
    Top = 108
    object mnFileEdit: TSpTBXItem
      Action = actProjectFileEdit
    end
    object SpTBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object mnFileRemove: TSpTBXItem
      Action = actProjectRemove
    end
    object SpTBXSeparatorItem6: TSpTBXSeparatorItem
    end
    object mnFileProperties: TSpTBXItem
      Action = actProjectFileProperties
    end
  end
  object ProjectRunSettingsPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 104
    Top = 109
    object mnAddRunConfig: TSpTBXItem
      Action = actProjectAddRunConfig
    end
  end
  object ProjectRunConfigPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 135
    Top = 109
    object mnRun: TSpTBXItem
      Action = actProjectRun
    end
    object mnDebug: TSpTBXItem
      Action = actProjectDebug
    end
    object mnExternalRun: TSpTBXItem
      Action = actProjectExternalRun
    end
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object mnEditRunConfig: TSpTBXItem
      Action = actProjectEditRunConfig
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object mnRenameRunConfig: TSpTBXItem
      Action = actProjectRename
    end
    object SpTBXSeparatorItem4: TSpTBXSeparatorItem
    end
    object mnRemoveRunConfig: TSpTBXItem
      Action = actProjectRemove
    end
  end
  object vilProjects: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 1
        CollectionName = 'Projects\Project'
        Disabled = False
        Name = 'Project'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Projects\Folder'
        Disabled = False
        Name = 'Folder'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Projects\RunConfigs'
        Disabled = False
        Name = 'RunConfigs'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Projects\RunConfig'
        Disabled = False
        Name = 'RunConfig'
      end>
    ImageCollection = icProjects
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 56
    Top = 160
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 25
        CollectionName = 'Item26'
        Disabled = False
        Name = 'Item26'
      end
      item
        CollectionIndex = 27
        CollectionName = 'Item28'
        Disabled = False
        Name = 'Item28'
      end
      item
        CollectionIndex = 28
        CollectionName = 'Item29'
        Disabled = False
        Name = 'Item29'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Item30'
        Disabled = False
        Name = 'Item30'
      end
      item
        CollectionIndex = 51
        CollectionName = 'Item52'
        Disabled = False
        Name = 'Item52'
      end
      item
        CollectionIndex = 53
        CollectionName = 'Item54'
        Disabled = False
        Name = 'Item54'
      end
      item
        CollectionIndex = 133
        CollectionName = 'Item134'
        Disabled = False
        Name = 'Item134'
      end
      item
        CollectionIndex = 134
        CollectionName = 'Item135'
        Disabled = False
        Name = 'Item135'
      end
      item
        CollectionIndex = 135
        CollectionName = 'Item136'
        Disabled = False
        Name = 'Item136'
      end
      item
        CollectionIndex = 136
        CollectionName = 'Item137'
        Disabled = False
        Name = 'Item137'
      end
      item
        CollectionIndex = 137
        CollectionName = 'Item138'
        Disabled = False
        Name = 'Item138'
      end
      item
        CollectionIndex = 139
        CollectionName = 'Item140'
        Disabled = False
        Name = 'Item140'
      end
      item
        CollectionIndex = 140
        CollectionName = 'Item141'
        Disabled = False
        Name = 'Item141'
      end
      item
        CollectionIndex = 141
        CollectionName = 'Item142'
        Disabled = False
        Name = 'Item142'
      end
      item
        CollectionIndex = 142
        CollectionName = 'Item143'
        Disabled = False
        Name = 'Item143'
      end
      item
        CollectionIndex = 143
        CollectionName = 'Item144'
        Disabled = False
        Name = 'Item144'
      end
      item
        CollectionIndex = 161
        CollectionName = 'Item162'
        Disabled = False
        Name = 'Item162'
      end
      item
        CollectionIndex = 144
        CollectionName = 'Item145'
        Disabled = False
        Name = 'Item145'
      end
      item
        CollectionIndex = 145
        CollectionName = 'Item146'
        Disabled = False
        Name = 'Item146'
      end
      item
        CollectionIndex = 146
        CollectionName = 'Item147'
        Disabled = False
        Name = 'Item147'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 106
    Top = 160
  end
  object icProjects: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Projects\Folder'
        SVGText = 
          '<svg viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg"><pat' +
          'h d="m28 7h-12l-3-3h-9c-1.7 0-3 1.4-3 3v18c0 1.7 1.3 3 3 3h24c1.' +
          '7 0 3-1.3 3-3v-15c0-1.7-1.4-3-3-3zm0 18h-24v-15h24z" transform="' +
          'matrix(1.1 0 0 1.2 -1.1 -2.4)"/><path d="m12 12h10v12h-10z" fill' +
          '="none" stroke="#000" stroke-width="3"/></svg>'
      end
      item
        IconName = 'Projects\Project'
        SVGText = 
          '<svg height="24" viewBox="0 0 24 24" width="24" xmlns="http://ww' +
          'w.w3.org/2000/svg"><path d="m19 14.87v-5.74c0-.72-.38-1.38-1-1.7' +
          '3l-5-2.88c-.31-.18-.65-.27-1-.27s-.69.09-1 .27l-5 2.87c-.62.36-1' +
          ' 1.02-1 1.74v5.74c0 .72.38 1.38 1 1.73l5 2.88c.31.18.65.27 1 .27' +
          's.69-.09 1-.27l5-2.88c.62-.35 1-1.01 1-1.73zm-8 2.3-4-2.3v-4.63l' +
          '4 2.33zm1-6.33-3.96-2.31 3.96-2.28 3.96 2.28zm5 4.03-4 2.3v-4.6l' +
          '4-2.33z" transform="matrix(1.5 0 0 1.5 -6 -6)"/></svg>'
      end
      item
        IconName = 'Projects\RunConfig'
        SVGText = 
          '<svg viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg" xmln' +
          's:xlink="http://www.w3.org/1999/xlink"><clipPath id="a"><path d=' +
          '"m0 0v32h20v-16h12v-16z"/></clipPath><path d="m30.7 23.6-6 6-2.1' +
          '-2.1 2.4-2.4h-6.3v-3h6.3l-2.4-2.4 2.1-2.1 2.9 2.8 2.6 2.7z" tran' +
          'sform="matrix(1.1 0 0 1.1 -2.2 -3.3)"/><g clip-path="url(#a)"><p' +
          'ath d="m19 14.87v-5.74c0-.72-.38-1.38-1-1.73l-5-2.88c-.31-.18-.6' +
          '5-.27-1-.27s-.69.09-1 .27l-5 2.87c-.62.36-1 1.02-1 1.74v5.74c0 .' +
          '72.38 1.38 1 1.73l5 2.88c.31.18.65.27 1 .27s.69-.09 1-.27l5-2.88' +
          'c.62-.35 1-1.01 1-1.73zm-8 2.3-4-2.3v-4.63l4 2.33zm1-6.33-3.96-2' +
          '.31 3.96-2.28 3.96 2.28zm5 4.03-4 2.3v-4.6l4-2.33z" transform="m' +
          'atrix(2 0 0 2 -8 -8)"/></g></svg>'
      end
      item
        IconName = 'Projects\RunConfigs'
        SVGText = 
          '<svg viewBox="0 0 32 32"><path d="m28 7h-12l-3-3h-9c-1.7 0-3 1.4' +
          '-3 3v18c0 1.7 1.3 3 3 3h24c1.7 0 3-1.3 3-3v-15c0-1.7-1.4-3-3-3zm' +
          '0 18h-24v-15h24z" transform="matrix(1.1 0 0 1.2 -1.1 -2.4)"/>'#13#10'.' +
          '...<path d="m16.3 16h-6.3v3h6.3l-2.4 2.4 2.1 2.1 6-6-6-6-2.1 2.1' +
          'z" transform="matrix(1.2 0 0 1.2 -2.4 -2.4)"/></svg>'
      end>
    Left = 10
    Top = 160
  end
end
