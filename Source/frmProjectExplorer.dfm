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
  TextHeight = 15
  inherited BGPanel: TPanel
    inherited FGPanel: TPanel
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 223
        Height = 394
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ExplorerTree: TVirtualStringTree
          Left = 0
          Top = 30
          Width = 223
          Height = 364
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
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <>
        end
        object SpTBXDock1: TSpTBXDock
          Left = 0
          Top = 0
          Width = 223
          Height = 30
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
      ImageName = 'ProjectFile'
      OnExecute = actProjectNewExecute
    end
    object actProjectOpen: TAction
      Category = 'Project'
      Caption = '&Open Project...'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Open a project file'
      ImageIndex = 7
      ImageName = 'ProjectOpen'
      OnExecute = actProjectOpenExecute
    end
    object actProjectSave: TAction
      Category = 'Project'
      Caption = '&Save Project'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Save the project'
      ImageIndex = 12
      ImageName = 'ProjectSave'
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
      ImageName = 'Folders'
      OnExecute = actProjectExtraPythonPathExecute
    end
    object actProjectExpandAll: TAction
      Category = 'Project'
      Caption = '&Expand All'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Expand all project nodes'
      ImageIndex = 2
      ImageName = 'Expand'
      OnExecute = actProjectExpandAllExecute
    end
    object actProjectCollapseAll: TAction
      Category = 'Project'
      Caption = '&Collapse All'
      HelpContext = 435
      HelpType = htContext
      Hint = 'Collapse all project nodes'
      ImageIndex = 3
      ImageName = 'Collapse'
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
      ImageName = 'ProjectAdd'
      OnExecute = actProjectAddFilesExecute
    end
    object actProjectAddFolder: TAction
      Category = 'Project'
      Caption = 'Add &Subfolder'
      HelpContext = 435
      Hint = 'Add a new subfolder'
      ImageIndex = 10
      ImageName = 'FolderAdd'
      OnExecute = actProjectAddFolderExecute
    end
    object actProjectRemove: TAction
      Category = 'Project'
      Caption = '&Remove'
      HelpContext = 435
      Hint = 'Remove a file, folder or run configuration from the project'
      ImageIndex = 9
      ImageName = 'ProjectRemove'
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
      ImageName = 'Editor'
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
      ImageName = 'RunConfigAdd'
      OnExecute = actProjectAddRunConfigExecute
    end
    object actProjectEditRunConfig: TAction
      Category = 'Project'
      Caption = '&Edit Run Configuration'
      HelpContext = 435
      Hint = 'Edit run configuration'
      ImageIndex = 14
      ImageName = 'RunConfigEdit'
      OnExecute = actProjectEditRunConfigExecute
    end
    object actProjectFileProperties: TAction
      Category = 'Project'
      Caption = '&Properties'
      HelpContext = 435
      Hint = 'Show file properties'
      ImageIndex = 15
      ImageName = 'FileProperties'
      OnExecute = actProjectFilePropertiesExecute
    end
    object actProjectRun: TAction
      Category = 'Project'
      Caption = '&Run'
      HelpContext = 435
      Hint = 'Run the selected configuration'
      ImageIndex = 4
      ImageName = 'Run'
      OnExecute = actProjectRunExecute
    end
    object actProjectExternalRun: TAction
      Category = 'Project'
      Caption = 'E&xternal Run'
      HelpContext = 435
      Hint = 'Run configuration using an external Python interpreter'
      ImageIndex = 1
      ImageName = 'ExternalRun'
      OnExecute = actProjectExternalRunExecute
    end
    object actProjectDebug: TAction
      Category = 'Project'
      Caption = '&Debug'
      HelpContext = 435
      Hint = 'Debug the selected configuration'
      ImageIndex = 5
      ImageName = 'Debug'
      OnExecute = actProjectDebugExecute
    end
    object actProjectAddRemoteFile: TAction
      Category = 'Project'
      Caption = 'Add Remote File'
      HelpContext = 435
      Hint = 'Add a remote file to the folder'
      ImageIndex = 16
      ImageName = 'Download'
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
    Images = <
      item
        CollectionIndex = 1
        CollectionName = 'Projects\Project'
        Name = 'Project'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Projects\Folder'
        Name = 'Folder'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Projects\RunConfigs'
        Name = 'RunConfigs'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Projects\RunConfig'
        Name = 'RunConfig'
      end>
    ImageCollection = icProjects
    PreserveItems = True
    Width = 18
    Height = 18
    Left = 56
    Top = 160
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 46
        CollectionName = 'Folders'
        Name = 'Folders'
      end
      item
        CollectionIndex = 32
        CollectionName = 'ExternalRun'
        Name = 'ExternalRun'
      end
      item
        CollectionIndex = 31
        CollectionName = 'Expand'
        Name = 'Expand'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Collapse'
        Name = 'Collapse'
      end
      item
        CollectionIndex = 93
        CollectionName = 'Run'
        Name = 'Run'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Debug'
        Name = 'Debug'
      end
      item
        CollectionIndex = 75
        CollectionName = 'ProjectFile'
        Name = 'ProjectFile'
      end
      item
        CollectionIndex = 76
        CollectionName = 'ProjectOpen'
        Name = 'ProjectOpen'
      end
      item
        CollectionIndex = 73
        CollectionName = 'ProjectAdd'
        Name = 'ProjectAdd'
      end
      item
        CollectionIndex = 77
        CollectionName = 'ProjectRemove'
        Name = 'ProjectRemove'
      end
      item
        CollectionIndex = 45
        CollectionName = 'FolderAdd'
        Name = 'FolderAdd'
      end
      item
        CollectionIndex = 26
        CollectionName = 'Editor'
        Name = 'Editor'
      end
      item
        CollectionIndex = 78
        CollectionName = 'ProjectSave'
        Name = 'ProjectSave'
      end
      item
        CollectionIndex = 94
        CollectionName = 'RunConfigAdd'
        Name = 'RunConfigAdd'
      end
      item
        CollectionIndex = 95
        CollectionName = 'RunConfigEdit'
        Name = 'RunConfigEdit'
      end
      item
        CollectionIndex = 39
        CollectionName = 'FileProperties'
        Name = 'FileProperties'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Download'
        Name = 'Download'
      end
      item
        CollectionIndex = 96
        CollectionName = 'RunLast'
        Name = 'RunLast'
      end
      item
        CollectionIndex = 19
        CollectionName = 'DebugLast'
        Name = 'DebugLast'
      end
      item
        CollectionIndex = 33
        CollectionName = 'ExternalRunLast'
        Name = 'ExternalRunLast'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 106
    Top = 160
  end
  object icProjects: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Projects\Folder'
        SVGText = 
          '<svg viewBox="0 0 36 36">'#13#10#9'<path d="M31.2,7.2H18l-3.3-3.6H4.8c-' +
          '1.9,0-3.3,1.7-3.3,3.6v21.6c0,2,1.4,3.6,3.3,3.6h26.4c1.9,0,3.3-1.' +
          '6,3.3-3.6v-18'#13#10#9#9'C34.5,8.8,33,7.2,31.2,7.2z M31.2,28.8H4.8v-18h2' +
          '6.4V28.8z"/>'#13#10#9'<path d="M22.6,26.5h-9.2c-0.8,0-1.5-0.7-1.5-1.5V1' +
          '4.6c0-0.8,0.7-1.5,1.5-1.5h9.2c0.8,0,1.5,0.7,1.5,1.5V25'#13#10#9#9'C24.1,' +
          '25.8,23.4,26.5,22.6,26.5z M14.9,23.5h6.2v-7.3h-6.2V23.5z"/>'#13#10'</s' +
          'vg>'
      end
      item
        IconName = 'Projects\Project'
        SVGText = 
          '<svg viewBox="0 0 36 36">'#13#10'<g transform="scale(1.5) translate(-4' +
          ',-4)">'#13#10#9'<path d="M25.2,19.8v-7.6c0-0.9-0.5-1.8-1.3-2.3l-6.6-3.8' +
          'c-0.4-0.2-0.9-0.4-1.3-0.4c-0.5,0-0.9,0.1-1.3,0.4L8.1,9.9'#13#10#9#9'c-0.' +
          '8,0.5-1.3,1.3-1.3,2.3v7.6c0,0.9,0.5,1.8,1.3,2.3l6.6,3.8c0.4,0.2,' +
          '0.9,0.4,1.3,0.4c0.5,0,0.9-0.1,1.3-0.4l6.6-3.8'#13#10#9#9'C24.7,21.6,25.2' +
          ',20.7,25.2,19.8z M14.7,22.8l-5.3-3v-6.1l5.3,3.1V22.8z M16,14.5l-' +
          '5.2-3l5.2-3l5.2,3L16,14.5z M22.6,19.8l-5.3,3'#13#10#9#9'v-6.1l5.3-3.1V19' +
          '.8z"/>'#13#10'</g>'#13#10'</svg>'
      end
      item
        IconName = 'Projects\RunConfig'
        SVGText = 
          '<svg viewBox="0 0 36 36">'#13#10#9'<defs>'#13#10#9#9'<clipPath id="cp">'#13#10#9#9#9'<po' +
          'lygon points="2.4,2.4 2.4,33.6 21.9,33.6 21.9,18 33.6,18 33.6,2.' +
          '4 '#9#9'"/>'#13#10#9#9'</clipPath>'#13#10#9'</defs>'#13#10#9'<path d="M33.2,24.5l-6.4,6.4l' +
          '-2.2-2.2l2.6-2.6h-6.7v-3.2h6.7l-2.6-2.6l2.2-2.2l3.1,3l2.8,2.9L33' +
          '.2,24.5z"/>'#13#10#9'<g clip-path="url(#cp)">'#13#10#9#9'<path d="M31.6,23.6V12' +
          '.4c0-1.4-0.7-2.7-1.9-3.4l-9.7-5.6c-0.6-0.4-1.3-0.5-1.9-0.5s-1.3,' +
          '0.2-1.9,0.5L6.3,9c-1.2,0.7-1.9,2-1.9,3.4'#13#10#9#9#9'v11.2c0,1.4,0.7,2.7' +
          ',1.9,3.4l9.7,5.6c0.6,0.4,1.3,0.5,1.9,0.5s1.3-0.2,1.9-0.5l9.7-5.6' +
          'C30.9,26.3,31.6,25,31.6,23.6z M16.1,28.1'#13#10#9#9#9'l-7.8-4.5v-9l7.8,4.' +
          '5V28.1z M18,15.7l-7.7-4.5L18,6.8l7.7,4.4L18,15.7z M27.7,23.6l-7.' +
          '8,4.5v-9l7.8-4.5V23.6z"/>'#13#10#9'</g>'#13#10'</svg>'
      end
      item
        IconName = 'Projects\RunConfigs'
        SVGText = 
          '<svg viewBox="0 0 36 36">'#13#10'<path d="M31.2,7.2H18l-3.3-3.6H4.8c-1' +
          '.9,0-3.3,1.7-3.3,3.6v21.6c0,2,1.4,3.6,3.3,3.6h26.4c1.9,0,3.3-1.6' +
          ',3.3-3.6v-18'#13#10#9'C34.5,8.8,33,7.2,31.2,7.2z M31.2,28.8H4.8v-18h26.' +
          '4V28.8z"/>'#13#10'<path d="M18.4,18h-7.6v3.6h7.6l-2.9,2.9L18,27l7.2-7.' +
          '2L18,12.6l-2.5,2.5L18.4,18z"/>'#13#10'</svg>'
      end>
    Left = 10
    Top = 160
  end
end
