object PyIDEMainForm: TPyIDEMainForm
  Left = 0
  Top = 115
  HelpContext = 100
  Caption = 'Python Scripter'
  ClientHeight = 527
  ClientWidth = 868
  Color = clWindow
  Ctl3D = False
  ParentFont = True
  Position = poDefault
  ShowHint = True
  StyleElements = [seFont, seClient]
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShortCut = FormShortCut
  OnShow = FormShow
  TextHeight = 15
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 502
    Width = 868
    Height = 25
    SizeGrip = False
    object lbStatusMessage: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      Options = [tboNoAutoHint]
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      Wrapping = twNone
      CustomWidth = 0
    end
    object SpTBXSeparatorItem22: TSpTBXSeparatorItem
    end
    object lbPythonVersion: TSpTBXLabelItem
      Hint = 'Python version'
      OnClick = lbPythonVersionClick
      Alignment = taCenter
      MinWidth = 130
    end
    object SpTBXSeparatorItem23: TSpTBXSeparatorItem
    end
    object lbPythonEngine: TSpTBXLabelItem
      Hint = 'Python engine type'
      OnClick = lbPythonEngineClick
      Alignment = taCenter
      MinWidth = 100
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object lbStatusCaret: TSpTBXLabelItem
      Hint = 'Text position'
      Wrapping = twEndEllipsis
      OnClick = lbStatusCaretClick
      Alignment = taCenter
      CustomWidth = 92
    end
    object SpTBXSeparatorItem6: TSpTBXSeparatorItem
    end
    object lbStatusModified: TSpTBXLabelItem
      Caption = ' '
      Wrapping = twEndEllipsis
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 80
    end
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object lbStatusOverwrite: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 90
    end
    object SpTBXSeparatorItem8: TSpTBXSeparatorItem
    end
    object lbStatusCaps: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 40
    end
    object SpTBXSeparatorItem9: TSpTBXSeparatorItem
    end
    object spiStatusLED: TSpTBXItem
      Hint = 'Ready'
      ImageIndex = 0
      ImageName = 'StatusLED'
      Images = vilIndicators
    end
    object spiLspLed: TSpTBXItem
      Hint = 'Ready'
      ImageIndex = 2
      ImageName = 'LspLED'
      Images = vilIndicators
    end
    object spiExternalToolsLED: TSpTBXItem
      Hint = 'External Tool Running'
      ImageIndex = 1
      ImageName = 'ExternalToolsLED'
      Images = vilIndicators
      Visible = False
    end
  end
  object BGPanel: TPanel
    Left = 9
    Top = 85
    Width = 850
    Height = 408
    Align = alClient
    BevelEdges = []
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    object TabControl1: TSpTBXTabControl
      Left = 0
      Top = 0
      Width = 846
      Height = 408
      Align = alClient
      PopupMenu = TabControlPopupMenu
      OnContextPopup = TabContolContextPopup
      ActiveTabIndex = -1
      Images = vilImages
      TabDragReorder = True
      TabPosition = ttpBottom
      OnActiveTabChange = TabControlActiveTabChange
      HiddenItems = <>
      object tbiRightAlign: TSpTBXRightAlignSpacerItem
        CustomWidth = 720
      end
      object tbiTabSep: TSpTBXSeparatorItem
      end
      object tbiTabFiles: TSpTBXSubmenuItem
        Tag = 1
        Hint = 'Select File'
        ImageIndex = 98
        ImageName = 'Tabs'
        Options = [tboDropdownArrow]
        LinkSubitems = mnFiles
      end
      object tbiScrollLeft: TSpTBXItem
        Tag = 1
        Hint = 'Scroll left'
        Enabled = False
        ImageIndex = 54
        ImageName = 'TabPrevious'
        Options = [tboToolbarStyle]
        OnClick = tbiScrollLeftClick
      end
      object tbiScrollRight: TSpTBXItem
        Tag = 1
        Hint = 'Scroll right'
        Enabled = False
        ImageIndex = 53
        ImageName = 'TabNext'
        Options = [tboToolbarStyle]
        OnClick = tbiScrollRightClick
        FontSettings.Name = 'Marlett'
      end
      object tbiTabClose: TSpTBXItem
        Tag = 1
        Action = CommandsDataModule.actFileClose
        Options = [tboToolbarStyle]
      end
    end
    object TabControl2: TSpTBXTabControl
      Left = 846
      Top = 0
      Width = 0
      Height = 408
      Align = alRight
      PopupMenu = TabControlPopupMenu
      Visible = False
      OnContextPopup = TabContolContextPopup
      ActiveTabIndex = -1
      Images = vilImages
      TabDragReorder = True
      TabPosition = ttpBottom
      OnActiveTabChange = TabControlActiveTabChange
      HiddenItems = <>
      object SpTBXRightAlignSpacerItem2: TSpTBXRightAlignSpacerItem
        CustomWidth = 0
      end
      object SpTBXSeparatorItem13: TSpTBXSeparatorItem
      end
      object tbiTabFiles2: TSpTBXSubmenuItem
        Tag = 2
        Hint = 'Select File'
        ImageIndex = 98
        ImageName = 'Tabs'
        Options = [tboDropdownArrow]
        LinkSubitems = mnFiles
      end
      object tbiScrollLeft2: TSpTBXItem
        Tag = 2
        Hint = 'Scroll left'
        Enabled = False
        ImageIndex = 54
        ImageName = 'TabPrevious'
        Options = [tboToolbarStyle]
        OnClick = tbiScrollLeftClick
      end
      object tbiScrollRight2: TSpTBXItem
        Tag = 2
        Hint = 'Scroll right'
        Enabled = False
        ImageIndex = 53
        ImageName = 'TabNext'
        Options = [tboToolbarStyle]
        OnClick = tbiScrollRightClick
        FontSettings.Name = 'Marlett'
      end
      object tbiTabClose2: TSpTBXItem
        Tag = 2
        Action = CommandsDataModule.actFileClose
        Options = [tboToolbarStyle]
      end
    end
    object TabSplitter: TSpTBXSplitter
      Left = 846
      Top = 0
      Width = 4
      Height = 408
      Cursor = crSizeWE
      Align = alRight
      ParentColor = False
      Visible = False
    end
  end
  object TBXDockTop: TSpTBXDock
    Left = 0
    Top = 0
    Width = 868
    Height = 85
    PopupMenu = ToolbarPopupMenu
    object MainMenu: TSpTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      Images = vilImages
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      Customizable = False
      MenuBar = True
      object FileMenu: TSpTBXSubmenuItem
        Caption = '&File'
        object TBXSubmenuItem5: TSpTBXSubmenuItem
          Caption = 'New'
          object mnNewModule: TSpTBXItem
            Action = actFileNewModule
          end
          object TBXSeparatorItem23: TSpTBXSeparatorItem
          end
          object mnNewFile: TSpTBXItem
            Action = actNewFile
          end
        end
        object mnFileOpen: TSpTBXItem
          Action = actFileOpen
        end
        object RecentSubmenu: TSpTBXSubmenuItem
          Caption = '&Recent Files'
          object tbiRecentFileList: TSpTBXMRUListItem
            HidePathExtension = False
            MaxItems = 6
            OnClick = tbiRecentFileListClick
          end
        end
        object SpTBXSeparatorItem21: TSpTBXSeparatorItem
        end
        object SpTBXItem12: TSpTBXItem
          Action = actRemoteFileOpen
        end
        object SpTBXItem13: TSpTBXItem
          Action = CommandsDataModule.actFileSaveToRemote
        end
        object N14: TSpTBXSeparatorItem
        end
        object mnFileSave: TSpTBXItem
          Action = CommandsDataModule.actFileSave
        end
        object mnFileSaveAs: TSpTBXItem
          Action = CommandsDataModule.actFileSaveAs
        end
        object mnFileReload: TSpTBXItem
          Action = CommandsDataModule.actFileReload
        end
        object mnFileClose: TSpTBXItem
          Action = CommandsDataModule.actFileClose
        end
        object N1: TSpTBXSeparatorItem
        end
        object mnFileSaveAll: TSpTBXItem
          Action = CommandsDataModule.actFileSaveAll
        end
        object mnFileCloseAll: TSpTBXItem
          Action = actFileCloseAll
        end
        object N2: TSpTBXSeparatorItem
        end
        object PageSetup1: TSpTBXItem
          Action = CommandsDataModule.actPageSetup
        end
        object PrinterSetup1: TSpTBXItem
          Action = CommandsDataModule.actPrinterSetup
        end
        object PrintPreview1: TSpTBXItem
          Action = CommandsDataModule.actPrintPreview
        end
        object Print1: TSpTBXItem
          Action = CommandsDataModule.actFilePrint
        end
        object N4: TSpTBXSeparatorItem
        end
        object N3: TSpTBXItem
          Action = actFileExit
        end
      end
      object EditMenu: TSpTBXSubmenuItem
        Caption = '&Edit'
        object mnEditUndo: TSpTBXItem
          Action = CommandsDataModule.actEditUndo
        end
        object mnEditRedo: TSpTBXItem
          Action = CommandsDataModule.actEditRedo
        end
        object N5: TSpTBXSeparatorItem
        end
        object mnEditCut: TSpTBXItem
          Action = CommandsDataModule.actEditCut
        end
        object mnEditCopy: TSpTBXItem
          Action = CommandsDataModule.actEditCopy
        end
        object mnEditPaste: TSpTBXItem
          Action = CommandsDataModule.actEditPaste
        end
        object mnEditDelete: TSpTBXItem
          Action = CommandsDataModule.actEditDelete
        end
        object mnEditSelectAll: TSpTBXItem
          Action = CommandsDataModule.actEditSelectAll
        end
        object SpTBXSeparatorItem19: TSpTBXSeparatorItem
        end
        object SpTBXItem10: TSpTBXItem
          Action = CommandsDataModule.actEditReadOnly
        end
        object TBXSeparatorItem9: TSpTBXSeparatorItem
        end
        object mnSourceCode: TSpTBXSubmenuItem
          Caption = '&Source Code'
          object mnIndentBlock: TSpTBXItem
            Action = CommandsDataModule.actEditIndent
          end
          object mnDedentBlock: TSpTBXItem
            Action = CommandsDataModule.actEditDedent
          end
          object mnToggleComment: TSpTBXItem
            Action = CommandsDataModule.actEditToggleComment
          end
          object mnTabify: TSpTBXItem
            Action = CommandsDataModule.actEditTabify
          end
          object mnUnTabify: TSpTBXItem
            Action = CommandsDataModule.actEditUntabify
          end
          object TBXSeparatorItem27: TSpTBXSeparatorItem
          end
          object mnExecSelection: TSpTBXItem
            Action = actExecSelection
          end
        end
        object SpTBXSeparatorItem10: TSpTBXSeparatorItem
        end
        object mnSpelling: TSpTBXSubmenuItem
          Caption = 'Spelling'
          ImageIndex = 102
          ImageName = 'SpellCheck'
          OnPopup = mnSpellingPopup
          object mnSpellCheckTopSeparator: TSpTBXSeparatorItem
          end
          object mnSpellCheckAdd: TSpTBXItem
            Action = CommandsDataModule.actSynSpellErrorAdd
          end
          object mnSpellCheckDelete: TSpTBXItem
            Action = CommandsDataModule.actSynSpellErrorDelete
          end
          object mnSpellCheckIgnore: TSpTBXItem
            Action = CommandsDataModule.actSynSpellErrorIgnore
          end
          object mnSpellCheckIgnoreOnce: TSpTBXItem
            Action = CommandsDataModule.actSynSpellErrorIgnoreOnce
          end
          object mnSpellCheckSecondSeparator: TSpTBXSeparatorItem
          end
          object SpTBXItem20: TSpTBXItem
            Action = CommandsDataModule.actSynSpellCheckWord
          end
          object SpTBXItem21: TSpTBXItem
            Action = CommandsDataModule.actSynSpellCheckLine
          end
          object SpTBXItem22: TSpTBXItem
            Action = CommandsDataModule.actSynSpellCheckSelection
          end
          object SpTBXItem23: TSpTBXItem
            Action = CommandsDataModule.actSynSpellCheckFile
          end
          object SpTBXSeparatorItem24: TSpTBXSeparatorItem
          end
          object SpTBXItem24: TSpTBXItem
            Action = CommandsDataModule.actSynSpellClearErrors
          end
          object SpTBXSeparatorItem25: TSpTBXSeparatorItem
          end
          object SpTBXItem25: TSpTBXItem
            Action = CommandsDataModule.actSynSpellCheckAsYouType
          end
        end
        object TBXSeparatorItem13: TSpTBXSeparatorItem
        end
        object Parameters1: TSpTBXSubmenuItem
          Caption = 'Parameters'
          object mnInsertParameter: TSpTBXItem
            Action = CommandsDataModule.actParameterCompletion
          end
          object mnInsertModifier: TSpTBXItem
            Action = CommandsDataModule.actModifierCompletion
          end
          object N16: TSpTBXSeparatorItem
          end
          object mnReplaceParameter: TSpTBXItem
            Action = CommandsDataModule.actReplaceParameters
          end
        end
        object mnIsertCodeTemplate: TSpTBXItem
          Action = CommandsDataModule.actInsertTemplate
        end
        object N6: TSpTBXSeparatorItem
        end
        object TBXSubmenuItem3: TSpTBXSubmenuItem
          Caption = 'File Format'
          object mnEditAnsi: TSpTBXItem
            Action = CommandsDataModule.actEditAnsi
            RadioItem = True
          end
          object mnEditUtf8NoBom: TSpTBXItem
            Action = CommandsDataModule.actEditUTF8NoBOM
            RadioItem = True
          end
          object mnEditUtf8: TSpTBXItem
            Action = CommandsDataModule.actEditUTF8
            RadioItem = True
          end
          object mnEditUtf16LE: TSpTBXItem
            Action = CommandsDataModule.actEditUTF16LE
          end
          object mnEditUtf16BE: TSpTBXItem
            Action = CommandsDataModule.actEditUTF16BE
          end
          object TBXSeparatorItem12: TSpTBXSeparatorItem
          end
          object mnEditLBDos: TSpTBXItem
            Action = CommandsDataModule.actEditLBDos
            RadioItem = True
          end
          object mnEditLBUnix: TSpTBXItem
            Action = CommandsDataModule.actEditLBUnix
            RadioItem = True
          end
          object mnEditLBMac: TSpTBXItem
            Action = CommandsDataModule.actEditLBMac
            RadioItem = True
          end
        end
      end
      object SearchMenu: TSpTBXSubmenuItem
        Caption = '&Search'
        object mnSearchFind: TSpTBXItem
          Action = CommandsDataModule.actSearchFind
        end
        object mnSearchFindNext: TSpTBXItem
          Action = CommandsDataModule.actSearchFindNext
        end
        object mnSearchFindPrevious: TSpTBXItem
          Action = CommandsDataModule.actSearchFindPrev
        end
        object mnSearchReplace: TSpTBXItem
          Action = CommandsDataModule.actSearchReplace
        end
        object mnSearchHighlight: TSpTBXItem
          Action = CommandsDataModule.actSearchHighlight
        end
        object N15: TSpTBXSeparatorItem
        end
        object mnFindinFiles: TSpTBXItem
          Action = CommandsDataModule.actFindInFiles
        end
        object N7: TSpTBXSeparatorItem
        end
        object mnGoToLine: TSpTBXItem
          Action = CommandsDataModule.actSearchGoToLine
        end
        object mnGotoSyntaxError: TSpTBXItem
          Action = CommandsDataModule.actSearchGoToSyntaxError
        end
        object mnGoToDebugLine: TSpTBXItem
          Action = CommandsDataModule.actSearchGoToDebugLine
        end
        object TBXSeparatorItem31: TSpTBXSeparatorItem
        end
        object mnFindFunction: TSpTBXItem
          Action = CommandsDataModule.actFindFunction
        end
        object mnFindNextReference: TSpTBXItem
          Action = CommandsDataModule.actFindNextReference
        end
        object mnFindPreviousReference: TSpTBXItem
          Action = CommandsDataModule.actFindPreviousReference
        end
        object mnMatchingBrace: TSpTBXItem
          Action = CommandsDataModule.actSearchMatchingBrace
        end
        object N23: TSpTBXSeparatorItem
        end
        object mnFindDefinition: TSpTBXItem
          Action = actFindDefinition
        end
        object mnFindReferences: TSpTBXItem
          Action = actFindReferences
        end
      end
      object ViewMenu: TSpTBXSubmenuItem
        Caption = '&View'
        object mnNextEditor: TSpTBXItem
          Action = actViewNextEditor
        end
        object mnPreviousEditor: TSpTBXItem
          Action = actViewPreviousEditor
        end
        object mnSplitEditors: TSpTBXSubmenuItem
          Caption = 'Split Editor'
          object mnSplitEditorVer: TSpTBXItem
            Action = actViewSplitEditorVer
          end
          object mnSplitEditorHor: TSpTBXItem
            Action = actViewSplitEditorHor
          end
          object SpTBXSeparatorItem15: TSpTBXSeparatorItem
          end
          object mnHideSecondEditor: TSpTBXItem
            Action = actViewHideSecondEditor
          end
        end
        object mnSplitWorkspace: TSpTBXSubmenuItem
          Caption = 'Split Workspace'
          object SpTBXItem8: TSpTBXItem
            Action = actViewSplitWorkspaceVer
          end
          object SpTBXItem7: TSpTBXItem
            Action = actViewSplitWorkspaceHor
          end
          object SpTBXSeparatorItem14: TSpTBXSeparatorItem
          end
          object SpTBXItem9: TSpTBXItem
            Action = actViewHideSecondaryWorkspace
          end
        end
        object TBXSeparatorItem20: TSpTBXSeparatorItem
        end
        object mnZoomIn: TSpTBXItem
          Action = actEditorZoomIn
        end
        object mnZoomOut: TSpTBXItem
          Action = actEditorZoomOut
        end
        object mnResetZoom: TSpTBXItem
          Action = actEditorZoomReset
        end
        object N10: TSpTBXSeparatorItem
        end
        object mnuToolbars: TSpTBXSubmenuItem
          Caption = '&Toolbars'
          object mnViewMainMenu: TSpTBXItem
            Action = actViewMainMenu
          end
          object mnMainToolbarVisibilityToggle: TSpTBXItem
            Caption = '&Main Toolbar'
            Hint = 'Main toolbar|Show/Hide the Main toolbar'
            HelpContext = 360
            Control = MainToolBar
          end
          object mnDebugtoolbarVisibilityToggle: TSpTBXItem
            Caption = '&Debug Toolbar'
            Hint = 'Debug toolbar|Show/Hide the Debug toolbar'
            HelpContext = 360
            Control = DebugToolbar
          end
          object mnEditorToolbarVisibilityToggle: TSpTBXItem
            Caption = '&Editor Toolbar'
            Hint = 'Editor toolbar|Show/Hide the Editor toolbar'
            HelpContext = 360
            Control = EditorToolbar
          end
          object mnViewToolbarVisibilityToggle: TSpTBXItem
            Caption = '&View Toolbar'
            Hint = 'View toolbar|Show/Hide the View toolbar'
            HelpContext = 360
            Control = ViewToolbar
          end
          object mnuUserToolbarVisibilityToggle: TSpTBXItem
            Caption = 'Use&r Toolbar'
            Hint = 'User toolbar|Show/Hide the User toolbar'
            HelpContext = 360
            Control = UserToolbar
          end
          object SpTBXSeparatorItem3: TSpTBXSeparatorItem
          end
          object mnViewCustomizeToolbars: TSpTBXItem
            Action = actViewCustomizeToolbars
          end
        end
        object mnViewStatusBar: TSpTBXItem
          Action = actViewStatusBar
        end
        object TBXSeparatorItem18: TSpTBXSeparatorItem
        end
        object TBXSubmenuItem4: TSpTBXSubmenuItem
          Caption = 'IDE &Windows'
          object mnViewII: TSpTBXItem
            Action = actViewII
          end
          object mnViewProjectExplorer: TSpTBXItem
            Action = actViewProjectExplorer
          end
          object mnViewFileExplorer: TSpTBXItem
            Action = actViewFileExplorer
          end
          object mnViewCodeExplorer: TSpTBXItem
            Action = actViewCodeExplorer
          end
          object mnViewToDoList: TSpTBXItem
            Action = actViewToDoList
          end
          object mnViewRegExpTester: TSpTBXItem
            Action = actViewRegExpTester
          end
          object mnViewFindResults: TSpTBXItem
            Action = actViewFindResults
          end
          object mnViewOutput: TSpTBXItem
            Action = actViewOutput
          end
          object mnViewUnitTests: TSpTBXItem
            Action = actViewUnitTests
          end
        end
        object DebugWindows1: TSpTBXSubmenuItem
          Caption = '&Debug Windows'
          object mnViewBreakpoints: TSpTBXItem
            Action = actBreakPointsWin
          end
          object mnViewCallStack: TSpTBXItem
            Action = actCallStackWin
          end
          object mnViewMessages: TSpTBXItem
            Action = actMessagesWin
          end
          object mnViewVariables: TSpTBXItem
            Action = actVariablesWin
          end
          object mnViewWatches: TSpTBXItem
            Action = actWatchesWin
          end
        end
        object TBXSubmenuItem6: TSpTBXSubmenuItem
          Caption = 'Navi&gate'
          object mnNavEditor: TSpTBXItem
            Action = actNavEditor
          end
          object mnNavInterpreter: TSpTBXItem
            Action = actNavInterpreter
          end
          object TBXSeparatorItem24: TSpTBXSeparatorItem
          end
          object mnNavCodeExplorer: TSpTBXItem
            Action = actNavCodeExplorer
          end
          object mnNavFileExplorer: TSpTBXItem
            Action = actNavFileExplorer
          end
          object mnNavProjectExplorer: TSpTBXItem
            Action = actNavProjectExplorer
          end
          object mnNavUnitTests: TSpTBXItem
            Action = actNavUnitTests
          end
          object mnNavOutput: TSpTBXItem
            Action = actNavOutput
          end
          object mnNavTodo: TSpTBXItem
            Action = actNavTodo
          end
          object TBXSeparatorItem25: TSpTBXSeparatorItem
          end
          object mnNavBreakpoints: TSpTBXItem
            Action = actNavBreakpoints
          end
          object mnNavCallStack: TSpTBXItem
            Action = actNavCallStack
          end
          object mnNavMessages: TSpTBXItem
            Action = actNavMessages
          end
          object mnNavVariables: TSpTBXItem
            Action = actNavVariables
          end
          object mnNavWatches: TSpTBXItem
            Action = actNavWatches
          end
        end
        object TBXSeparatorItem16: TSpTBXSeparatorItem
        end
        object mnSyntax: TSpTBXSubmenuItem
          Caption = '&Syntax'
          OnPopup = mnSyntaxPopup
          object TBXSeparatorItem19: TSpTBXSeparatorItem
          end
          object mnNoSyntax: TSpTBXItem
            Caption = '&No Syntax'
            Hint = 'Do not use syntax highlighting'
            OnClick = mnSyntaxClick
          end
        end
        object TBXSeparatorItem21: TSpTBXSeparatorItem
        end
        object mnLanguage: TSpTBXSubmenuItem
          Caption = 'Language'
        end
        object mnLayouts: TSpTBXSubmenuItem
          Caption = 'Layouts'
          ImageIndex = 97
          ImageName = 'Layouts'
          object mnLayOutSeparator: TSpTBXSeparatorItem
          end
          object TBXItem47: TSpTBXItem
            Caption = 'Save Layout...'
            Hint = 'Save Current Layout'
            OnClick = actLayoutSaveExecute
          end
          object TBXItem48: TSpTBXItem
            Caption = 'Delete Layouts...'
            Hint = 'Delete a layout'
            OnClick = actLayoutsDeleteExecute
          end
          object TBXItem49: TSpTBXItem
            Caption = 'Set Debug Layout'
            Hint = 'Set the current layout as the debug layout'
            OnClick = actLayoutDebugExecute
          end
          object TBXSeparatorItem17: TSpTBXSeparatorItem
          end
          object mnMaximizeEditor: TSpTBXItem
            Action = actMaximizeEditor
          end
          object TBXItem77: TSpTBXItem
            Action = actRestoreEditor
          end
        end
        object SpTBXItem5: TSpTBXItem
          Action = actSelectStyle
        end
      end
      object ProjectMenu: TSpTBXSubmenuItem
        Caption = '&Project'
        object mnProjectNew: TSpTBXItem
          Action = ProjectExplorerWindow.actProjectNew
          Images = ProjectExplorerWindow.vilImages
        end
        object mnProjectOpen: TSpTBXItem
          Action = ProjectExplorerWindow.actProjectOpen
          Images = ProjectExplorerWindow.vilImages
        end
        object SpTBXSubmenuItem1: TSpTBXSubmenuItem
          Caption = '&Recent Projects'
          object tbiRecentProjects: TSpTBXMRUListItem
            HidePathExtension = False
            MaxItems = 6
            OnClick = tbiRecentProjectsClick
          end
        end
        object SpTBXSeparatorItem16: TSpTBXSeparatorItem
        end
        object mnProjectSave: TSpTBXItem
          Action = ProjectExplorerWindow.actProjectSave
          Images = ProjectExplorerWindow.vilImages
        end
        object mnProjectSaveAs: TSpTBXItem
          Action = ProjectExplorerWindow.actProjectSaveAs
        end
        object SpTBXSeparatorItem4: TSpTBXSeparatorItem
        end
        object mnNavProjectExplorer2: TSpTBXItem
          Action = actNavProjectExplorer
        end
      end
      object RunMenu: TSpTBXSubmenuItem
        Caption = '&Run'
        object mnSyntaxCheck: TSpTBXItem
          Action = actSyntaxCheck
        end
        object mnImportModule: TSpTBXItem
          Action = actImportModule
        end
        object N21: TSpTBXSeparatorItem
        end
        object mnRun: TSpTBXItem
          Action = actRun
        end
        object mnCommandLineParams: TSpTBXItem
          Action = actCommandLine
        end
        object N22: TSpTBXSeparatorItem
        end
        object mnExternalRun: TSpTBXItem
          Action = actExternalRun
        end
        object mnConfigureExternalRun: TSpTBXItem
          Action = actExternalRunConfigure
        end
        object N8: TSpTBXSeparatorItem
        end
        object mnDebug: TSpTBXItem
          Action = actDebug
        end
        object mnRunToCursor: TSpTBXItem
          Action = actRunToCursor
        end
        object mnStepInto: TSpTBXItem
          Action = actStepInto
        end
        object mnStepOver: TSpTBXItem
          Action = actStepOver
        end
        object mnStepOut: TSpTBXItem
          Action = actStepOut
        end
        object mnPause: TSpTBXItem
          Action = actDebugPause
        end
        object mnAbortDebugging: TSpTBXItem
          Action = actDebugAbort
        end
        object TBXSeparatorItem33: TSpTBXSeparatorItem
        end
        object mnPostMortem: TSpTBXItem
          Action = actPostMortem
        end
        object N9: TSpTBXSeparatorItem
        end
        object mnTogglebreakpoint: TSpTBXItem
          Action = actToggleBreakPoint
        end
        object mnClearAllBreakpoints: TSpTBXItem
          Action = actClearAllBreakpoints
        end
        object mnAddWatchAtCursor: TSpTBXItem
          Action = actAddWatchAtCursor
        end
        object SpTBXSeparatorItem17: TSpTBXSeparatorItem
        end
        object mnPythonVersions: TSpTBXSubmenuItem
          Caption = 'Python Versions'
          ImageIndex = 83
          ImageName = 'Python'
          OnPopup = mnPythonVersionsPopup
          object SpTBXSeparatorItem18: TSpTBXSeparatorItem
          end
          object SpTBXItem4: TSpTBXItem
            Action = actPythonSetup
            Images = vilImages
          end
        end
        object mnPythonEngines: TSpTBXSubmenuItem
          Caption = 'Python Engine'
          object mnEngineInternal: TSpTBXItem
            Action = actPythonInternal
          end
          object mnEngineRemote: TSpTBXItem
            Action = actPythonRemote
          end
          object mnEngineRemoteTk: TSpTBXItem
            Action = actPythonRemoteTk
          end
          object mnEngineRemoteWx: TSpTBXItem
            Action = actPythonRemoteWx
          end
          object mnPythonEngineSSH: TSpTBXItem
            Action = actPythonSSH
          end
          object TBXSeparatorItem26: TSpTBXSeparatorItem
          end
          object mnReinitEngine: TSpTBXItem
            Action = actPythonReinitialize
          end
        end
      end
      object ToolsMenu: TSpTBXSubmenuItem
        Caption = '&Tools'
        object mnPythonPath: TSpTBXItem
          Action = CommandsDataModule.actPythonPath
        end
        object mnUnitTestWizard: TSpTBXItem
          Action = CommandsDataModule.actUnitTestWizard
        end
        object EditorViewsMenu: TSpTBXSubmenuItem
          Caption = 'Source Code Views'
          OnClick = EditorViewsMenuClick
        end
        object N13: TSpTBXSeparatorItem
        end
        object mnTools: TSpTBXSubmenuItem
          Caption = 'Tools'
          Hint = 'External Tools'
          ImageIndex = 55
          ImageName = 'Tools'
        end
        object mnConfigureTools: TSpTBXItem
          Action = CommandsDataModule.actConfigureTools
        end
        object N20: TSpTBXSeparatorItem
        end
        object OptionsMenu: TSpTBXSubmenuItem
          Caption = '&Options'
          object mnIDEOptions: TSpTBXItem
            Action = CommandsDataModule.actIDEOptions
          end
          object mnIDEShortCuts: TSpTBXItem
            Action = CommandsDataModule.actIDEShortcuts
          end
          object mnEditorOptions: TSpTBXItem
            Action = CommandsDataModule.actEditorOptions
          end
          object TBXSeparatorItem29: TSpTBXSeparatorItem
          end
          object TBXSubmenuItem7: TSpTBXSubmenuItem
            Caption = 'Import/Export'
            object mnExportShortcuts: TSpTBXItem
              Action = CommandsDataModule.actExportShortCuts
            end
            object mnImportShortcuts: TSpTBXItem
              Action = CommandsDataModule.actImportShortcuts
            end
            object TBXSeparatorItem30: TSpTBXSeparatorItem
            end
            object mnExportHighlighters: TSpTBXItem
              Action = CommandsDataModule.actExportHighlighters
            end
            object mnImportHighlighters: TSpTBXItem
              Action = CommandsDataModule.actImportHighlighters
            end
          end
          object TBXSeparatorItem8: TSpTBXSeparatorItem
          end
          object mnCustomizeParameters: TSpTBXItem
            Action = CommandsDataModule.actCustomizeParameters
          end
          object mnCodeTemplates: TSpTBXItem
            Action = CommandsDataModule.actCodeTemplates
          end
          object mnFileTemplates: TSpTBXItem
            Action = CommandsDataModule.actFileTemplates
          end
        end
        object TBXSeparatorItem15: TSpTBXSeparatorItem
        end
        object SpTBXItem2: TSpTBXItem
          Action = CommandsDataModule.actToolsEditStartupScripts
        end
        object SpTBXItem15: TSpTBXItem
          Action = CommandsDataModule.actToolsRestartLS
        end
        object SpTBXSeparatorItem12: TSpTBXSeparatorItem
        end
        object mnCheckForUpdates: TSpTBXItem
          Action = CommandsDataModule.actCheckForUpdates
        end
      end
      object HelpMenu: TSpTBXSubmenuItem
        Caption = '&Help'
        object mnHelpPythonManuals: TSpTBXItem
          Action = CommandsDataModule.actPythonManuals
        end
        object N18: TSpTBXSeparatorItem
        end
        object PyScripter1: TSpTBXSubmenuItem
          Caption = 'PyScripter'
          object mnHelpContents: TSpTBXItem
            Action = CommandsDataModule.actHelpContents
          end
          object mnHelpEditorShortcuts: TSpTBXItem
            Action = CommandsDataModule.actHelpEditorShortcuts
          end
          object mnHelpExternalTools: TSpTBXItem
            Action = CommandsDataModule.actHelpExternalTools
          end
          object mnHelpParameters: TSpTBXItem
            Action = CommandsDataModule.actHelpParameters
          end
        end
        object SpTBXSubmenuItem2: TSpTBXSubmenuItem
          Caption = 'Web Support'
          object mnHelpProjectHome: TSpTBXItem
            Action = CommandsDataModule.actHelpWebProjectHome
          end
          object mnHelpWebSupport: TSpTBXItem
            Action = CommandsDataModule.actHelpWebGroupSupport
          end
          object SpTBXItem3: TSpTBXItem
            Action = CommandsDataModule.actHelpWebBlog
          end
        end
        object SpTBXItem14: TSpTBXItem
          Action = CommandsDataModule.actDonate
        end
        object N17: TSpTBXSeparatorItem
        end
        object mnHelpAbout: TSpTBXItem
          Action = CommandsDataModule.actAbout
        end
      end
    end
    object MainToolBar: TSpTBXToolbar
      Left = 0
      Top = 25
      DockPos = -6
      DockRow = 1
      Images = vilImages
      TabOrder = 1
      Caption = 'Main Toolbar'
      object tbiFileNewModule: TSpTBXItem
        Action = actFileNewModule
      end
      object tbiFileOpen: TSpTBXItem
        Action = actFileOpen
      end
      object tbiFileSave: TSpTBXItem
        Action = CommandsDataModule.actFileSave
      end
      object tbiFileSaveAll: TSpTBXItem
        Action = CommandsDataModule.actFileSaveAll
      end
      object TBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object tbiFilePrint: TSpTBXItem
        Action = CommandsDataModule.actFilePrint
      end
      object TBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object tbiEditCut: TSpTBXItem
        Action = CommandsDataModule.actEditCut
      end
      object tbiEditCopy: TSpTBXItem
        Action = CommandsDataModule.actEditCopy
      end
      object tbiEditPaste: TSpTBXItem
        Action = CommandsDataModule.actEditPaste
      end
      object TBXSeparatorItem3: TSpTBXSeparatorItem
      end
      object tbiEditUndo: TSpTBXItem
        Action = CommandsDataModule.actEditUndo
      end
      object tbiEditRedo: TSpTBXItem
        Action = CommandsDataModule.actEditRedo
      end
      object TBXSeparatorItem4: TSpTBXSeparatorItem
      end
      object tbiSearchFind: TSpTBXItem
        Action = CommandsDataModule.actSearchFind
      end
      object tbiSearchFindNext: TSpTBXItem
        Action = CommandsDataModule.actSearchFindNext
      end
      object tbiSearchReplace: TSpTBXItem
        Action = CommandsDataModule.actSearchReplace
      end
      object tbiFindInFiles: TSpTBXItem
        Action = CommandsDataModule.actFindInFiles
      end
      object mnuFindInFilesResults: TSpTBXItem
        Action = actViewFindResults
      end
      object TBXSeparatorItem5: TSpTBXSeparatorItem
      end
      object tbiAbout: TSpTBXItem
        Action = CommandsDataModule.actAbout
      end
    end
    object DebugToolbar: TSpTBXToolbar
      Left = 361
      Top = 25
      DockPos = 361
      DockRow = 1
      Images = vilImages
      TabOrder = 2
      Caption = 'Debug Toolbar'
      object tbiRunRun: TSpTBXItem
        Action = actRun
      end
      object TBXSeparatorItem6: TSpTBXSeparatorItem
      end
      object tbiRunDebug: TSpTBXItem
        Action = actDebug
      end
      object tbiRunRunToCursor: TSpTBXItem
        Action = actRunToCursor
      end
      object tbiRunStepInto: TSpTBXItem
        Action = actStepInto
      end
      object tbiRunStepOver: TSpTBXItem
        Action = actStepOver
      end
      object tbiRunStepOut: TSpTBXItem
        Action = actStepOut
      end
      object tbiRunPause: TSpTBXItem
        Action = actDebugPause
      end
      object tbiRunAbort: TSpTBXItem
        Action = actDebugAbort
      end
      object TBXSeparatorItem7: TSpTBXSeparatorItem
      end
      object tbiRunToggleBreakpoint: TSpTBXItem
        Action = actToggleBreakPoint
      end
      object tbiRunClearAllBreakpoints: TSpTBXItem
        Action = actClearAllBreakpoints
      end
    end
    object ViewToolbar: TSpTBXToolbar
      Left = 769
      Top = 25
      DockPos = 728
      DockRow = 1
      Images = vilImages
      TabOrder = 4
      Caption = 'View Toolbar'
      object tbiSelectPythonVersion: TSpTBXSubmenuItem
        Caption = 'Python Versions'
        ImageIndex = 83
        ImageName = 'Python'
        LinkSubitems = mnPythonVersions
      end
      object tbiSelectStyle: TSpTBXItem
        Action = actSelectStyle
      end
      object tbiViewLayouts: TSpTBXSubmenuItem
        Caption = 'Layouts'
        Hint = 'Layouts'
        ImageIndex = 97
        ImageName = 'Layouts'
        Options = [tboDropdownArrow]
        LinkSubitems = mnLayouts
      end
    end
    object EditorToolbar: TSpTBXToolbar
      Left = 574
      Top = 25
      DockPos = 574
      DockRow = 1
      Images = vilImages
      TabOrder = 3
      Caption = 'Editor Toolbar'
      object tbiBrowsePrevious: TSpTBXSubmenuItem
        Action = actBrowseBack
        Options = [tboDropdownArrow]
        DropdownCombo = True
        object mnPreviousList: TSpTBXMRUListItem
          HidePathExtension = False
          MaxItems = 15
          OnClick = PreviousListClick
        end
      end
      object tbiBrowseNext: TSpTBXSubmenuItem
        Action = actBrowseForward
        Options = [tboDropdownArrow]
        DropdownCombo = True
        object mnNextList: TSpTBXMRUListItem
          HidePathExtension = False
          MaxItems = 15
          OnClick = NextListClick
        end
      end
      object TBXSeparatorItem14: TSpTBXSeparatorItem
      end
      object tbiEditDedent: TSpTBXItem
        Action = CommandsDataModule.actEditDedent
      end
      object tbiEditIndent: TSpTBXItem
        Action = CommandsDataModule.actEditIndent
      end
      object TBXSeparatorItem10: TSpTBXSeparatorItem
      end
      object tbiEditToggleComment: TSpTBXItem
        Action = CommandsDataModule.actEditToggleComment
      end
      object TBXSeparatorItem11: TSpTBXSeparatorItem
      end
      object tbiEditLineNumbers: TSpTBXItem
        Action = CommandsDataModule.actEditLineNumbers
      end
      object tbiEditWordWrap: TSpTBXItem
        Action = CommandsDataModule.actEditWordWrap
      end
      object tbiEditSpecialCharacters: TSpTBXItem
        Action = CommandsDataModule.actEditShowSpecialChars
      end
    end
    object FindToolbar: TSpTBXToolbar
      Left = 0
      Top = 55
      CloseButtonWhenDocked = True
      DockPos = -5
      DockRow = 2
      Images = vilImages
      Options = [tboDropdownArrow]
      TabOrder = 5
      Visible = False
      OnVisibleChanged = FindToolbarVisibleChanged
      Caption = 'Find Toolbar'
      Customizable = False
      object tbiFindLabel: TSpTBXLabelItem
        Caption = 'Find:'
      end
      object TBControlItem2: TTBControlItem
        Control = tbiSearchText
      end
      object tbiFindNext: TSpTBXItem
        Action = CommandsDataModule.actSearchFindNext
      end
      object tbiFindPrevious: TSpTBXItem
        Action = CommandsDataModule.actSearchFindPrev
      end
      object tbiHighlight: TSpTBXItem
        Action = CommandsDataModule.actSearchHighlight
        RadioItem = True
      end
      object tbiReplaceSeparator: TSpTBXSeparatorItem
        Visible = False
      end
      object tbiReplaceLabel: TSpTBXLabelItem
        Caption = 'Replace with:'
        Visible = False
      end
      object TBControlItem4: TTBControlItem
        Control = tbiReplaceText
      end
      object tbiReplaceExecute: TSpTBXItem
        Action = CommandsDataModule.actSearchReplaceNow
      end
      object TBXSeparatorItem32: TSpTBXSeparatorItem
      end
      object tbiSearchOptions: TSpTBXSubmenuItem
        Caption = 'Options'
        ImageIndex = 101
        ImageName = 'Setup'
        OnPopup = tbiSearchOptionsPopup
        object tbiSearchFromCaret: TSpTBXItem
          Caption = 'Search From C&aret'
          AutoCheck = True
          OnClick = SearchOptionsChanged
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
        object tbiAutoCaseSensitive: TSpTBXItem
          Caption = '&Auto Case Sensitive'
          Hint = 'Case Sensitive when search text contains upper case characters'
          AutoCheck = True
          Checked = True
          OnClick = SearchOptionsChanged
        end
        object tbiCaseSensitive: TSpTBXItem
          Caption = '&Case Sensitive'
          AutoCheck = True
          OnClick = SearchOptionsChanged
        end
        object tbiWholeWords: TSpTBXItem
          Caption = '&Whole Words Only'
          AutoCheck = True
          OnClick = SearchOptionsChanged
        end
        object tbiSearchInSelection: TSpTBXItem
          Caption = 'Search in &Selection'
          AutoCheck = True
          OnClick = SearchOptionsChanged
        end
        object tbiRegExp: TSpTBXItem
          Caption = '&Regular Expressions'
          AutoCheck = True
          OnClick = SearchOptionsChanged
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object tbiIncrementalSearch: TSpTBXItem
          Caption = '&Incremental Search'
          AutoCheck = True
          OnClick = SearchOptionsChanged
        end
      end
      object tbiSearchText: TSpTBXComboBox
        Left = 36
        Top = 1
        Width = 160
        Height = 23
        AutoDropDownWidth = True
        AutoCloseUp = True
        ItemHeight = 15
        TabOrder = 0
        OnChange = tbiSearchTextChange
        OnExit = tbiSearchTextExit
        OnKeyPress = tbiSearchTextKeyPress
      end
      object tbiReplaceText: TSpTBXComboBox
        Left = 277
        Top = 1
        Width = 160
        Height = 23
        AutoDropDownWidth = True
        ItemHeight = 15
        TabOrder = 1
        Visible = False
        OnChange = tbiReplaceTextChange
        OnKeyPress = tbiReplaceTextKeyPress
      end
    end
    object UserToolbar: TSpTBXToolbar
      Left = 515
      Top = 55
      DockPos = 310
      DockRow = 2
      Images = vilImages
      TabOrder = 6
      Visible = False
      Caption = 'User Toolbar'
    end
  end
  object TBXDockLeft: TSpTBXDock
    Left = 0
    Top = 85
    Width = 9
    Height = 408
    FixAlign = True
    PopupMenu = ToolbarPopupMenu
    Position = dpLeft
    DoubleBuffered = True
  end
  object TBXDockRight: TSpTBXDock
    Left = 859
    Top = 85
    Width = 9
    Height = 408
    FixAlign = True
    PopupMenu = ToolbarPopupMenu
    Position = dpRight
    DoubleBuffered = True
  end
  object TBXDockBottom: TSpTBXDock
    Left = 0
    Top = 493
    Width = 868
    Height = 9
    FixAlign = True
    PopupMenu = ToolbarPopupMenu
    Position = dpBottom
    DoubleBuffered = True
  end
  object DockServer: TJvDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    LeftSplitterStyle.ResizeStyle = rsUpdate
    LeftSplitterStyle.Size = 5
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    RightSplitterStyle.ResizeStyle = rsUpdate
    RightSplitterStyle.Size = 5
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    TopSplitterStyle.ResizeStyle = rsUpdate
    TopSplitterStyle.Size = 5
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    BottomSplitterStyle.ResizeStyle = rsUpdate
    BottomSplitterStyle.Size = 5
    Left = 37
    Top = 158
  end
  object AppStorage: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.DateTimeAsString = False
    StorageOptions.DefaultIfReadConvertError = True
    StorageOptions.StoreDefaultValues = False
    FlushOnDestroy = False
    Location = flCustom
    DefaultSection = 'Other Settings'
    SubStorages = <>
    Left = 394
    Top = 93
  end
  object TabControlPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 336
    Top = 282
    object mnNewModule2: TSpTBXItem
      Action = actFileNewModule
    end
    object mnFileClose2: TSpTBXItem
      Action = CommandsDataModule.actFileClose
    end
    object mnFileCloseAll2: TSpTBXItem
      Action = actFileCloseAll
    end
    object mnFileCloseAllOther: TSpTBXItem
      Action = CommandsDataModule.actFileCloseAllOther
    end
    object SpTBXItem6: TSpTBXItem
      Action = CommandsDataModule.actFileCloseAllToTheRight
    end
    object SpTBXSeparatorItem20: TSpTBXSeparatorItem
    end
    object SpTBXItem11: TSpTBXItem
      Action = CommandsDataModule.actEditReadOnly
    end
    object TBXSeparatorItem28: TSpTBXSeparatorItem
    end
    object SpTBXItem1: TSpTBXItem
      Action = CommandsDataModule.actEditCopyFileName
    end
    object SpTBXSeparatorItem11: TSpTBXSeparatorItem
    end
    object mnMaximizeEditor2: TSpTBXItem
      Action = actMaximizeEditor
    end
    object mnRestoreEditor2: TSpTBXItem
      Action = actRestoreEditor
    end
    object N12: TSpTBXSeparatorItem
    end
    object mnEditorOptions2: TSpTBXItem
      Action = CommandsDataModule.actEditorOptions
    end
    object TBXSeparatorItem22: TSpTBXSeparatorItem
    end
    object mnFiles: TSpTBXSubmenuItem
      Caption = '&Files'
      Hint = 'Open Files'
      OnClick = mnFilesClick
    end
  end
  object RunningProcessesPopUpMenu: TSpTBXPopupMenu
    LinkSubitems = OutputWindow.RunningProcess
    Left = 188
    Top = 282
  end
  object JvAppInstances: TJvAppInstances
    Active = False
    OnCmdLineReceived = JvAppInstancesCmdLineReceived
    Left = 467
    Top = 93
  end
  object SpTBXCustomizer: TSpTBXCustomizer
    Images = vilImages
    OnGetCustomizeForm = SpTBXCustomizerGetCustomizeForm
    Left = 280
    Top = 96
  end
  object ToolbarPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    LinkSubitems = mnuToolbars
    Left = 40
    Top = 282
  end
  object actlImmutable: TActionList
    Images = vilImages
    Left = 112
    Top = 100
    object actViewNextEditor: TAction
      Category = 'View'
      Caption = '&Next Editor'
      HelpContext = 360
      Hint = 'Next Editor|Move to the next editor'
      ImageIndex = 53
      ImageName = 'TabNext'
      ShortCut = 16393
      OnExecute = actNextEditorExecute
    end
    object actViewPreviousEditor: TAction
      Category = 'View'
      Caption = '&Previous Editor'
      HelpContext = 360
      Hint = 'Previous editor|Move to the previous editor'
      ImageIndex = 54
      ImageName = 'TabPrevious'
      ShortCut = 24585
      OnExecute = actPreviousEditorExecute
    end
  end
  object actlStandard: TActionList
    Images = vilImages
    Left = 42
    Top = 100
    object actViewMainMenu: TAction
      Category = 'View'
      Caption = 'Main Men&u'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide main menu'
      ShortCut = 16505
      OnExecute = actViewMainMenuExecute
    end
    object actCallStackWin: TAction
      Category = 'Debug Windows'
      Caption = '&Call Stack'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Call Stack window'
      ImageIndex = 41
      ImageName = 'CallStack'
      OnExecute = actCallStackWinExecute
    end
    object actVariablesWin: TAction
      Category = 'Debug Windows'
      Caption = '&Variables'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Variables window'
      ImageIndex = 42
      ImageName = 'VariablesWin'
      OnExecute = actVariablesWinExecute
    end
    object actSyntaxCheck: TAction
      Category = 'Run'
      Caption = '&Syntax Check'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Syntax Check|Perform syntax check and load scripts'
      ImageIndex = 17
      ImageName = 'Check'
      OnExecute = actSyntaxCheckExecute
    end
    object actRun: TAction
      Category = 'Run'
      Caption = '&Run'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Run|Run active module'
      ImageIndex = 33
      ImageName = 'Run'
      ShortCut = 16504
      OnExecute = actRunExecute
    end
    object actCommandLine: TAction
      Category = 'Run'
      Caption = 'Command Line &Parameters...'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Set command line parameters'
      OnExecute = actCommandLineExecute
    end
    object actImportModule: TAction
      Category = 'Run'
      Caption = 'Import &Module'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Import module'
      ImageIndex = 28
      ImageName = 'RunScript'
      OnExecute = actImportModuleExecute
    end
    object actFileNewModule: TAction
      Category = 'File'
      Caption = '&New Python module'
      HelpContext = 310
      HelpType = htContext
      Hint = 'New Python module'
      ImageIndex = 84
      ImageName = 'PythonScript'
      ShortCut = 16462
      OnExecute = actFileNewModuleExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      HelpContext = 310
      HelpType = htContext
      Hint = 'Select a file to open'
      ImageIndex = 1
      ImageName = 'FileOpen'
      ShortCut = 16463
      OnExecute = actFileOpenExecute
    end
    object actFileCloseAll: TAction
      Category = 'File'
      Caption = 'Close A&ll'
      Enabled = False
      HelpContext = 310
      HelpType = htContext
      Hint = 'Close all files'
      ImageIndex = 93
      ImageName = 'TabsClose'
      ShortCut = 24691
      OnExecute = actFileCloseAllExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      HelpContext = 310
      HelpType = htContext
      Hint = 'Exit'
      ImageIndex = 32
      ImageName = 'Exit'
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actViewStatusBar: TAction
      Category = 'View'
      Caption = 'Status &Bar'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide status bar'
      OnExecute = actViewStatusBarExecute
    end
    object actExternalRun: TAction
      Category = 'Run'
      Caption = 'E&xternal Run'
      HelpContext = 340
      HelpType = htContext
      Hint = 'External Run|Run active module in external Python interpreter'
      ImageIndex = 22
      ImageName = 'ExternalRun'
      ShortCut = 32888
      OnExecute = actExternalRunExecute
    end
    object actExternalRunConfigure: TAction
      Category = 'Run'
      Caption = 'Configure &External Run...'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Configure External Run'
      ImageIndex = 61
      ImageName = 'ExternalRunSetup'
      OnExecute = actExternalRunConfigureExecute
    end
    object actRunDebugLastScript: TAction
      Category = 'Run'
      Caption = 'Debug Last Script'
      HelpType = htContext
      Hint = 'Debug last script'
      ImageIndex = 88
      ImageName = 'DebugLast'
      ShortCut = 8312
      OnExecute = actRunDebugLastScriptExecute
    end
    object actDebug: TAction
      Category = 'Run'
      Caption = '&Debug'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Debug|Debug active script'
      ImageIndex = 34
      ImageName = 'Debug'
      ShortCut = 120
      OnExecute = actDebugExecute
    end
    object actRestoreEditor: TAction
      Category = 'View'
      Caption = '&Restore Editor'
      HelpContext = 270
      HelpType = htContext
      Hint = 'Restore editor window'
      ImageIndex = 75
      ImageName = 'EditorMin'
      ShortCut = 41050
      OnExecute = actRestoreEditorExecute
    end
    object actRunToCursor: TAction
      Category = 'Run'
      Caption = 'Run To &Cursor'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Run to cursor'
      ImageIndex = 35
      ImageName = 'RunToCursor'
      ShortCut = 115
      OnExecute = actRunToCursorExecute
    end
    object actStepInto: TAction
      Category = 'Run'
      Caption = 'Step &Into'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Step into subroutine '
      ImageIndex = 36
      ImageName = 'StepIn'
      ShortCut = 118
      OnExecute = actStepIntoExecute
    end
    object actStepOver: TAction
      Category = 'Run'
      Caption = 'Step &Over'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Step over next function call'
      ImageIndex = 37
      ImageName = 'StepOver'
      ShortCut = 119
      OnExecute = actStepOverExecute
    end
    object actStepOut: TAction
      Category = 'Run'
      Caption = 'Step O&ut'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Step out of the current subroutine'
      ImageIndex = 38
      ImageName = 'StepOut'
      ShortCut = 8311
      OnExecute = actStepOutExecute
    end
    object actDebugPause: TAction
      Category = 'Run'
      Caption = '&Pause'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Pause running program'
      ImageIndex = 73
      ImageName = 'Pause'
      OnExecute = actDebugPauseExecute
    end
    object actDebugAbort: TAction
      Category = 'Run'
      Caption = '&Abort Debugging'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Abort debugging'
      ImageIndex = 30
      ImageName = 'Abort'
      ShortCut = 49272
      OnExecute = actDebugAbortExecute
    end
    object actRunLastScriptExternal: TAction
      Category = 'Run'
      Caption = 'Run Last Script Externally'
      HelpType = htContext
      Hint = 'Run last script externally'
      ImageIndex = 89
      ImageName = 'ExternalRunLast'
      ShortCut = 41080
      OnExecute = actRunLastScriptExternalExecute
    end
    object actRunLastScript: TAction
      Category = 'Run'
      Caption = 'Run Last Script'
      HelpType = htContext
      Hint = 'Run last script'
      ImageIndex = 87
      ImageName = 'RunLast'
      ShortCut = 24696
      OnExecute = actRunLastScriptExecute
    end
    object actToggleBreakPoint: TAction
      Category = 'Run'
      Caption = 'Toggle &breakpoint'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Toggle breakpoint'
      ImageIndex = 39
      ImageName = 'Breakpoint'
      ShortCut = 116
      OnExecute = actToggleBreakPointExecute
    end
    object actClearAllBreakpoints: TAction
      Category = 'Run'
      Caption = 'Clear A&ll Breakpoints'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Clear all breakpoints'
      ImageIndex = 40
      ImageName = 'BreakpointsRemove'
      OnExecute = actClearAllBreakpointsExecute
    end
    object actBreakPointsWin: TAction
      Category = 'Debug Windows'
      Caption = '&Breakpoints'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Breakpoints window'
      ImageIndex = 44
      ImageName = 'BreakpointsWin'
      OnExecute = actBreakPointsWinExecute
    end
    object actWatchesWin: TAction
      Category = 'Debug Windows'
      Caption = '&Watches'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Watches window'
      ImageIndex = 43
      ImageName = 'WatchesWin'
      OnExecute = actWatchesWinExecute
    end
    object actMessagesWin: TAction
      Category = 'Debug Windows'
      Caption = '&Messages'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Messages window'
      ImageIndex = 49
      ImageName = 'MessagesWin'
      OnExecute = actMessagesWinExecute
    end
    object actViewII: TAction
      Category = 'View'
      Caption = '&Interactive Interpreter'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Interactive Interpreter'
      ImageIndex = 83
      ImageName = 'Python'
      OnExecute = actViewIIExecute
    end
    object actViewCodeExplorer: TAction
      Category = 'View'
      Caption = '&Code Explorer'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Code Explorer'
      ImageIndex = 50
      ImageName = 'CodeExplorer'
      OnExecute = actViewCodeExplorerExecute
    end
    object actViewFileExplorer: TAction
      Category = 'View'
      Caption = '&File Explorer'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide File Explorer'
      ImageIndex = 57
      ImageName = 'FileExplorer'
      OnExecute = actViewFileExplorerExecute
    end
    object actViewToDoList: TAction
      Category = 'View'
      Caption = '&To-Do List'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide To Do List'
      ImageIndex = 58
      ImageName = 'TodoWin'
      OnExecute = actViewToDoListExecute
    end
    object actViewFindResults: TAction
      Category = 'View'
      Caption = 'Find &in Files Results'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Find in Files Results'
      ImageIndex = 60
      ImageName = 'FindResults'
      ShortCut = 49222
      OnExecute = actViewFindResultsExecute
    end
    object actViewOutput: TAction
      Category = 'View'
      Caption = '&Output Window'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Output Window'
      ImageIndex = 62
      ImageName = 'CmdOuputWin'
      OnExecute = actViewOutputExecute
    end
    object actViewUnitTests: TAction
      Category = 'View'
      Caption = '&Unit Tests'
      HelpType = htContext
      Hint = 'View/Hide Unit Tests Window'
      ImageIndex = 68
      ImageName = 'UnitTestWin'
      OnExecute = actViewUnitTestsExecute
    end
    object actFindDefinition: TAction
      Category = 'Refactoring'
      Caption = 'Find &Definition'
      HelpContext = 830
      HelpType = htContext
      Hint = 'Find definition of an Identifier'
      ShortCut = 32836
      OnExecute = actFindDefinitionExecute
    end
    object actFindReferences: TAction
      Category = 'Refactoring'
      Caption = 'Find R&eferences'
      HelpContext = 840
      HelpType = htContext
      Hint = 'Find references of an Identifier'
      OnExecute = actFindReferencesExecute
    end
    object actBrowseBack: TAction
      Category = 'Refactoring'
      Caption = 'Browse &Back'
      HelpContext = 830
      HelpType = htContext
      Hint = 'Browse back'
      ImageIndex = 64
      ImageName = 'ArrowLeft'
      ShortCut = 32805
      OnExecute = tbiBrowsePreviousClick
    end
    object actBrowseForward: TAction
      Category = 'Refactoring'
      Caption = 'Browse &Forward'
      HelpContext = 830
      HelpType = htContext
      Hint = 'Browse forward'
      ImageIndex = 65
      ImageName = 'ArrowRight'
      ShortCut = 32807
      OnExecute = tbiBrowseNextClick
    end
    object actViewRegExpTester: TAction
      Category = 'View'
      Caption = '&Regular Expression Tester'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Regular Expression Tester'
      ImageIndex = 66
      ImageName = 'RegExp'
      OnExecute = actViewRegExpTesterExecute
    end
    object actLayoutSave: TAction
      Category = 'View'
      Caption = 'Save Layout...'
      HelpType = htContext
      Hint = 'Save Current Layout'
      OnExecute = actLayoutSaveExecute
    end
    object actLayoutsDelete: TAction
      Category = 'View'
      Caption = 'Delete Layouts...'
      HelpType = htContext
      Hint = 'Delete a layout'
      OnExecute = actLayoutsDeleteExecute
    end
    object actLayoutDebug: TAction
      Category = 'View'
      Caption = 'Set Debug Layout'
      HelpType = htContext
      Hint = 'Set the current layout as the debug layout'
      OnExecute = actLayoutDebugExecute
    end
    object actMaximizeEditor: TAction
      Category = 'View'
      Caption = '&Maximize Editor'
      HelpContext = 270
      HelpType = htContext
      Hint = 'Maximize editor window'
      ImageIndex = 74
      ImageName = 'EditorMax'
      ShortCut = 32858
      OnExecute = actMaximizeEditorExecute
    end
    object actEditorZoomIn: TAction
      Category = 'View'
      Caption = 'Zoom &In'
      HelpType = htContext
      Hint = 'Increase the font size of the editor'
      ImageIndex = 71
      ImageName = 'ZoomIn'
      ShortCut = 32875
      OnExecute = actEditorZoomInExecute
    end
    object actEditorZoomOut: TAction
      Category = 'View'
      Caption = 'Zoom &Out'
      HelpType = htContext
      Hint = 'Decrease the font size of the editor'
      ImageIndex = 72
      ImageName = 'ZoomOut'
      ShortCut = 32877
      OnExecute = actEditorZoomOutExecute
    end
    object actViewSplitEditorVer: TAction
      Category = 'View'
      Caption = 'Split Editor Vertically'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Split the editor Windows vertically'
      ImageIndex = 80
      ImageName = 'SplitVertical'
      OnExecute = actViewSplitEditorVerExecute
    end
    object actAddWatchAtCursor: TAction
      Category = 'Run'
      Caption = 'Add &Watch At Cursor'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Add the expression at the editor current position as a watch'
      ImageIndex = 43
      ImageName = 'WatchesWin'
      ShortCut = 32855
      OnExecute = actAddWatchAtCursorExecute
    end
    object actPythonReinitialize: TAction
      Category = 'Run'
      Caption = 'Reinitiali&ze Python engine'
      HelpContext = 340
      HelpType = htContext
      ShortCut = 16497
      OnExecute = actPythonReinitializeExecute
    end
    object actPythonInternal: TAction
      Category = 'Run'
      AutoCheck = True
      Caption = '&Internal'
      Checked = True
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use internal Python Engine'
      OnExecute = actPythonEngineExecute
    end
    object actPythonRemote: TAction
      Tag = 1
      Category = 'Run'
      AutoCheck = True
      Caption = '&Remote'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use a remote Python engine'
      OnExecute = actPythonEngineExecute
    end
    object actPythonRemoteTk: TAction
      Tag = 2
      Category = 'Run'
      AutoCheck = True
      Caption = 'Remote (&Tk)'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use a remote Python engine for Tkinter applications'
      OnExecute = actPythonEngineExecute
    end
    object actPythonRemoteWx: TAction
      Tag = 3
      Category = 'Run'
      AutoCheck = True
      Caption = 'Remote (&Wx)'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use a remote Python engine for wxPython applications'
      OnExecute = actPythonEngineExecute
    end
    object actNewFile: TAction
      Category = 'File'
      Caption = 'New File...'
      HelpType = htContext
      Hint = 'New file from template'
      ImageIndex = 0
      ImageName = 'FileNew'
      OnExecute = actNewFileExecute
    end
    object actNavWatches: TAction
      Category = 'IDE Navigation'
      Caption = '&Watches'
      HelpType = htContext
      Hint = 'Activate the Watches window'
      ImageIndex = 43
      ImageName = 'WatchesWin'
      ShortCut = 49239
      OnExecute = actNavWatchesExecute
    end
    object actNavBreakpoints: TAction
      Category = 'IDE Navigation'
      Caption = '&BreakPoints'
      HelpType = htContext
      Hint = 'Activate the Breakpoints window'
      ImageIndex = 44
      ImageName = 'BreakpointsWin'
      ShortCut = 49218
      OnExecute = actNavBreakpointsExecute
    end
    object actNavInterpreter: TAction
      Category = 'IDE Navigation'
      Caption = '&Interpreter'
      HelpType = htContext
      Hint = 'Activate the Interpreter window'
      ImageIndex = 83
      ImageName = 'Python'
      ShortCut = 49225
      OnExecute = actNavInterpreterExecute
    end
    object actNavVariables: TAction
      Category = 'IDE Navigation'
      Caption = '&Variables'
      HelpType = htContext
      Hint = 'Activate the Variables window'
      ImageIndex = 42
      ImageName = 'VariablesWin'
      ShortCut = 49238
      OnExecute = actNavVariablesExecute
    end
    object actNavCallStack: TAction
      Category = 'IDE Navigation'
      Caption = '&Call Stack'
      HelpType = htContext
      Hint = 'Activate the Call Stack window'
      ImageIndex = 41
      ImageName = 'CallStack'
      ShortCut = 49235
      OnExecute = actNavCallStackExecute
    end
    object actNavMessages: TAction
      Category = 'IDE Navigation'
      Caption = '&Messages '
      HelpType = htContext
      Hint = 'Activate the Messages window'
      ImageIndex = 49
      ImageName = 'MessagesWin'
      ShortCut = 49229
      OnExecute = actNavMessagesExecute
    end
    object actNavFileExplorer: TAction
      Category = 'IDE Navigation'
      Caption = 'File E&xplorer'
      HelpType = htContext
      Hint = 'Activate the File Explorer window'
      ImageIndex = 57
      ImageName = 'FileExplorer'
      ShortCut = 49240
      OnExecute = actNavFileExplorerExecute
    end
    object actNavCodeExplorer: TAction
      Category = 'IDE Navigation'
      Caption = '&Code Explorer'
      HelpType = htContext
      Hint = 'Activate the Code Explorer window'
      ImageIndex = 50
      ImageName = 'CodeExplorer'
      ShortCut = 49219
      OnExecute = actNavCodeExplorerExecute
    end
    object actNavTodo: TAction
      Category = 'IDE Navigation'
      Caption = '&Todo List'
      HelpType = htContext
      Hint = 'Activate the Todo List window'
      ImageIndex = 58
      ImageName = 'TodoWin'
      ShortCut = 49236
      OnExecute = actNavTodoExecute
    end
    object actNavUnitTests: TAction
      Category = 'IDE Navigation'
      Caption = '&Unit Tests'
      HelpType = htContext
      Hint = 'Activate the Todo List window'
      ImageIndex = 68
      ImageName = 'UnitTestWin'
      ShortCut = 49237
      OnExecute = actNavUnitTestsExecute
    end
    object actNavOutput: TAction
      Category = 'IDE Navigation'
      Caption = 'Command &Output'
      HelpType = htContext
      Hint = 'Activate the Command Output window'
      ImageIndex = 62
      ImageName = 'CmdOuputWin'
      ShortCut = 49231
      OnExecute = actNavOutputExecute
    end
    object actNavEditor: TAction
      Category = 'IDE Navigation'
      Caption = '&Editor'
      HelpType = htContext
      Hint = 'Activate the Editor'
      ImageIndex = 86
      ImageName = 'Editor'
      ShortCut = 123
      OnExecute = actNavEditorExecute
    end
    object actPythonSSH: TAction
      Tag = 4
      Category = 'Run'
      AutoCheck = True
      Caption = '&SSH'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use an SSH remote Python engine'
      OnExecute = actPythonEngineExecute
    end
    object actExecSelection: TAction
      Category = 'Run'
      Caption = 'E&xecute selection'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Execute the editor selection'
      ImageIndex = 16
      ImageName = 'Execute'
      ShortCut = 16502
      OnExecute = actExecSelectionExecute
    end
    object actViewSplitEditorHor: TAction
      Category = 'View'
      Caption = 'Split Editor Horizontally'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Split the editor Windows horizontally'
      ImageIndex = 81
      ImageName = 'SplitHorizontal'
      OnExecute = actViewSplitEditorHorExecute
    end
    object actViewHideSecondEditor: TAction
      Category = 'View'
      Caption = 'Hide Second Editor'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Clear the editor'
      OnExecute = actViewHideSecondEditorExecute
    end
    object actPostMortem: TAction
      Category = 'Run'
      Caption = '&Post Mortem'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Enter post mortem analysis'
      ImageIndex = 82
      ImageName = 'PostMortem'
      OnExecute = actPostMortemExecute
    end
    object actViewCustomizeToolbars: TAction
      Category = 'View'
      Caption = 'Customize...'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Customize the toolbars'
      OnExecute = actViewCustomizeToolbarsExecute
    end
    object actViewProjectExplorer: TAction
      Category = 'View'
      Caption = '&Project Explorer'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Project Explorer'
      ImageIndex = 85
      ImageName = 'ProjectExplorer'
      OnExecute = actViewProjectExplorerExecute
    end
    object actNavProjectExplorer: TAction
      Category = 'IDE Navigation'
      Caption = '&Project Explorer'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Activate the Project Explorer window'
      ImageIndex = 85
      ImageName = 'ProjectExplorer'
      ShortCut = 49232
      OnExecute = actNavProjectExplorerExecute
    end
    object actViewSplitWorkspaceVer: TAction
      Category = 'View'
      Caption = 'Split Workspace Vertically'
      HelpContext = 360
      HelpType = htContext
      Hint = 
        'Split workspace vertically|Show secondary editor tabset vertical' +
        'ly aligned'
      ImageIndex = 80
      ImageName = 'SplitVertical'
      OnExecute = actViewSplitWorkspaceVerExecute
    end
    object actViewSplitWorkspaceHor: TAction
      Category = 'View'
      Caption = 'Split Workspace Horizontally'
      HelpContext = 360
      HelpType = htContext
      Hint = 
        'Split workspace horizontally|Show secondary workspace horizontal' +
        'ly aligned'
      ImageIndex = 81
      ImageName = 'SplitHorizontal'
      OnExecute = actViewSplitWorkspaceHorExecute
    end
    object actViewHideSecondaryWorkspace: TAction
      Category = 'View'
      Caption = 'Hide Secondary Workspace'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Hide secondary workspace'
      OnExecute = actViewHideSecondaryWorkspaceExecute
    end
    object actSelectStyle: TAction
      Category = 'View'
      Caption = 'Styles...'
      Hint = 'Select Style'
      ImageIndex = 69
      ImageName = 'Styles'
      OnExecute = actSelectStyleExecute
    end
    object actPythonSetup: TAction
      Category = 'Run'
      Caption = 'Setup Python Versions...'
      Hint = 'Setup Python engine'
      ImageIndex = 94
      ImageName = 'PySetup'
      OnExecute = actPythonSetupExecute
    end
    object actRemoteFileOpen: TAction
      Category = 'File'
      Caption = 'Open Remote File'
      Hint = 'Open Remote File with SSH'
      ImageIndex = 95
      ImageName = 'Download'
      OnExecute = actRemoteFileOpenExecute
    end
    object actEditorZoomReset: TAction
      Category = 'View'
      Caption = '&Reset Zoom'
      HelpType = htContext
      Hint = 'Reset the font size of the editor to its default'
      ImageIndex = 103
      ImageName = 'ZoomReset'
      ShortCut = 32864
      OnExecute = actEditorZoomResetExecute
    end
  end
  object LocalAppStorage: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.DateTimeAsString = False
    StorageOptions.DefaultIfReadConvertError = True
    FlushOnDestroy = False
    Location = flCustom
    DefaultSection = 'Other Settings'
    SubStorages = <>
    Left = 394
    Top = 141
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 37
        CollectionName = 'FileNew'
        Name = 'FileNew'
      end
      item
        CollectionIndex = 38
        CollectionName = 'FileOpen'
        Name = 'FileOpen'
      end
      item
        CollectionIndex = 99
        CollectionName = 'Save'
        Name = 'Save'
      end
      item
        CollectionIndex = 100
        CollectionName = 'SaveAll'
        Name = 'SaveAll'
      end
      item
        CollectionIndex = 72
        CollectionName = 'PrintSetup'
        Name = 'PrintSetup'
      end
      item
        CollectionIndex = 71
        CollectionName = 'PrintPreview'
        Name = 'PrintPreview'
      end
      item
        CollectionIndex = 70
        CollectionName = 'Print'
        Name = 'Print'
      end
      item
        CollectionIndex = 128
        CollectionName = 'Undo'
        Name = 'Undo'
      end
      item
        CollectionIndex = 87
        CollectionName = 'Redo'
        Name = 'Redo'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Cut'
        Name = 'Cut'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 65
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 101
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 41
        CollectionName = 'FindNext'
        Name = 'FindNext'
      end
      item
        CollectionIndex = 91
        CollectionName = 'Replace'
        Name = 'Replace'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Execute'
        Name = 'Execute'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Check'
        Name = 'Check'
      end
      item
        CollectionIndex = 27
        CollectionName = 'EditOptions'
        Name = 'EditOptions'
      end
      item
        CollectionIndex = 1
        CollectionName = 'AppSettings'
        Name = 'AppSettings'
      end
      item
        CollectionIndex = 46
        CollectionName = 'Folders'
        Name = 'Folders'
      end
      item
        CollectionIndex = 49
        CollectionName = 'Function'
        Name = 'Function'
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
        CollectionIndex = 55
        CollectionName = 'Info'
        Name = 'Info'
      end
      item
        CollectionIndex = 51
        CollectionName = 'GoToLine'
        Name = 'GoToLine'
      end
      item
        CollectionIndex = 52
        CollectionName = 'Help'
        Name = 'Help'
      end
      item
        CollectionIndex = 97
        CollectionName = 'RunScript'
        Name = 'RunScript'
      end
      item
        CollectionIndex = 88
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Abort'
        Name = 'Abort'
      end
      item
        CollectionIndex = 58
        CollectionName = 'LineNumbers'
        Name = 'LineNumbers'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Exit'
        Name = 'Exit'
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
        CollectionIndex = 98
        CollectionName = 'RunToCursor'
        Name = 'RunToCursor'
      end
      item
        CollectionIndex = 108
        CollectionName = 'StepIn'
        Name = 'StepIn'
      end
      item
        CollectionIndex = 110
        CollectionName = 'StepOver'
        Name = 'StepOver'
      end
      item
        CollectionIndex = 109
        CollectionName = 'StepOut'
        Name = 'StepOut'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Breakpoint'
        Name = 'Breakpoint'
      end
      item
        CollectionIndex = 7
        CollectionName = 'BreakpointsRemove'
        Name = 'BreakpointsRemove'
      end
      item
        CollectionIndex = 10
        CollectionName = 'CallStack'
        Name = 'CallStack'
      end
      item
        CollectionIndex = 132
        CollectionName = 'VariablesWin'
        Name = 'VariablesWin'
      end
      item
        CollectionIndex = 136
        CollectionName = 'WatchesWin'
        Name = 'WatchesWin'
      end
      item
        CollectionIndex = 8
        CollectionName = 'BreakpointsWin'
        Name = 'BreakpointsWin'
      end
      item
        CollectionIndex = 54
        CollectionName = 'Indent'
        Name = 'Indent'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Dedent'
        Name = 'Dedent'
      end
      item
        CollectionIndex = 13
        CollectionName = 'CodeComment'
        Name = 'CodeComment'
      end
      item
        CollectionIndex = 127
        CollectionName = 'UnCodeComment'
        Name = 'UnCodeComment'
      end
      item
        CollectionIndex = 61
        CollectionName = 'MessagesWin'
        Name = 'MessagesWin'
      end
      item
        CollectionIndex = 14
        CollectionName = 'CodeExplorer'
        Name = 'CodeExplorer'
      end
      item
        CollectionIndex = 80
        CollectionName = 'PyDoc'
        Name = 'PyDoc'
      end
      item
        CollectionIndex = 64
        CollectionName = 'PageSetup'
        Name = 'PageSetup'
      end
      item
        CollectionIndex = 114
        CollectionName = 'TabNext'
        Name = 'TabNext'
      end
      item
        CollectionIndex = 115
        CollectionName = 'TabPrevious'
        Name = 'TabPrevious'
      end
      item
        CollectionIndex = 123
        CollectionName = 'Tools'
        Name = 'Tools'
      end
      item
        CollectionIndex = 124
        CollectionName = 'ToolsSetup'
        Name = 'ToolsSetup'
      end
      item
        CollectionIndex = 36
        CollectionName = 'FileExplorer'
        Name = 'FileExplorer'
      end
      item
        CollectionIndex = 121
        CollectionName = 'TodoWin'
        Name = 'TodoWin'
      end
      item
        CollectionIndex = 102
        CollectionName = 'SearchFolder'
        Name = 'SearchFolder'
      end
      item
        CollectionIndex = 44
        CollectionName = 'FindResults'
        Name = 'FindResults'
      end
      item
        CollectionIndex = 34
        CollectionName = 'ExternalRunSetup'
        Name = 'ExternalRunSetup'
      end
      item
        CollectionIndex = 12
        CollectionName = 'CmdOuputWin'
        Name = 'CmdOuputWin'
      end
      item
        CollectionIndex = 104
        CollectionName = 'SpecialChars'
        Name = 'SpecialChars'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ArrowLeft'
        Name = 'ArrowLeft'
      end
      item
        CollectionIndex = 3
        CollectionName = 'ArrowRight'
        Name = 'ArrowRight'
      end
      item
        CollectionIndex = 89
        CollectionName = 'RegExp'
        Name = 'RegExp'
      end
      item
        CollectionIndex = 56
        CollectionName = 'Keyboard'
        Name = 'Keyboard'
      end
      item
        CollectionIndex = 129
        CollectionName = 'UnitTestWin'
        Name = 'UnitTestWin'
      end
      item
        CollectionIndex = 112
        CollectionName = 'Styles'
        Name = 'Styles'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Assembly'
        Name = 'Assembly'
      end
      item
        CollectionIndex = 139
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end
      item
        CollectionIndex = 140
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 66
        CollectionName = 'Pause'
        Name = 'Pause'
      end
      item
        CollectionIndex = 25
        CollectionName = 'EditorMax'
        Name = 'EditorMax'
      end
      item
        CollectionIndex = 28
        CollectionName = 'EditorMin'
        Name = 'EditorMin'
      end
      item
        CollectionIndex = 42
        CollectionName = 'FindPrevious'
        Name = 'FindPrevious'
      end
      item
        CollectionIndex = 53
        CollectionName = 'Highlight'
        Name = 'Highlight'
      end
      item
        CollectionIndex = 50
        CollectionName = 'GoToError'
        Name = 'GoToError'
      end
      item
        CollectionIndex = 138
        CollectionName = 'WordWrap'
        Name = 'WordWrap'
      end
      item
        CollectionIndex = 107
        CollectionName = 'SplitVertical'
        Name = 'SplitVertical'
      end
      item
        CollectionIndex = 106
        CollectionName = 'SplitHorizontal'
        Name = 'SplitHorizontal'
      end
      item
        CollectionIndex = 69
        CollectionName = 'PostMortem'
        Name = 'PostMortem'
      end
      item
        CollectionIndex = 84
        CollectionName = 'Python'
        Name = 'Python'
      end
      item
        CollectionIndex = 85
        CollectionName = 'PythonScript'
        Name = 'PythonScript'
      end
      item
        CollectionIndex = 74
        CollectionName = 'ProjectExplorer'
        Name = 'ProjectExplorer'
      end
      item
        CollectionIndex = 26
        CollectionName = 'Editor'
        Name = 'Editor'
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
      end
      item
        CollectionIndex = 59
        CollectionName = 'Link'
        Name = 'Link'
      end
      item
        CollectionIndex = 137
        CollectionName = 'Web'
        Name = 'Web'
      end
      item
        CollectionIndex = 113
        CollectionName = 'TabClose'
        Name = 'TabCLose'
      end
      item
        CollectionIndex = 117
        CollectionName = 'TabsClose'
        Name = 'TabsClose'
      end
      item
        CollectionIndex = 81
        CollectionName = 'PySetup'
        Name = 'PySetup'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Download'
        Name = 'Download'
      end
      item
        CollectionIndex = 131
        CollectionName = 'Upload'
        Name = 'Upload'
      end
      item
        CollectionIndex = 57
        CollectionName = 'Layouts'
        Name = 'Layouts'
      end
      item
        CollectionIndex = 116
        CollectionName = 'Tabs'
        Name = 'Tabs'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Bug'
        Name = 'Bug'
      end
      item
        CollectionIndex = 24
        CollectionName = 'Edit'
        Name = 'Edit'
      end
      item
        CollectionIndex = 103
        CollectionName = 'Setup'
        Name = 'Setup'
      end
      item
        CollectionIndex = 105
        CollectionName = 'SpellCheck'
        Name = 'SpellCheck'
      end
      item
        CollectionIndex = 141
        CollectionName = 'ZoomReset'
        Name = 'ZoomReset'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 40
    Top = 344
  end
  object icIndicators: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'StatusLED'
        SVGText = 
          '<svg viewBox="0 0 100 100">'#13#10'  <defs>'#13#10'    <radialGradient cx=".' +
          '5" cy=".5"  fx=".3" fy=".2" r="0.5" id="myGradient" gradientUnit' +
          's="objectBoundingBox">'#13#10'      <stop offset="0%" stop-opacity="0.' +
          '3" stop-color="#22AA22"/>'#13#10'      <stop offset="1" stop-opacity="' +
          '1" stop-color="#22AA22"/>'#13#10'    </radialGradient>'#13#10'  </defs>'#13#10#13#10' ' +
          ' <!-- using my radial gradient 4CBB17 -->'#13#10'  <circle cx="50" cy=' +
          '"50" r="40" fill="url(#myGradient)" />'#13#10'</svg>'
      end
      item
        IconName = 'ExternalToolsLED'
        SVGText = 
          '<svg viewBox="0 0 100 100">'#13#10'  <defs>'#13#10'    <radialGradient cx=".' +
          '5" cy=".5"  fx=".3" fy=".2" r="0.5" id="myGradient" gradientUnit' +
          's="objectBoundingBox">'#13#10'      <stop offset="0%" stop-opacity="0.' +
          '3" stop-color="#E24444"/>'#13#10'      <stop offset="1" stop-opacity="' +
          '1" stop-color="#E24444"/>'#13#10'    </radialGradient>'#13#10'  </defs>'#13#10#13#10' ' +
          ' <!-- using my radial gradient -->'#13#10'  <circle cx="50" cy="50" r=' +
          '"40" fill="url(#myGradient)" />'#13#10'</svg>'
      end
      item
        IconName = 'LspLED'
        SVGText = 
          '<svg viewBox="0 0 100 100">'#13#10'  <defs>'#13#10'    <radialGradient cx=".' +
          '5" cy=".5"  fx=".3" fy=".2" r="0.5" id="myGradient" gradientUnit' +
          's="objectBoundingBox">'#13#10'      <stop offset="0%" stop-opacity="0.' +
          '3" stop-color="#FF5F1F"/>'#13#10'      <stop offset="1" stop-opacity="' +
          '1" stop-color="#FF5F1F"/>'#13#10'    </radialGradient>'#13#10'  </defs>'#13#10#13#10' ' +
          ' <!-- using my radial gradient 4CBB17 -->'#13#10'  <circle cx="50" cy=' +
          '"50" r="40" fill="url(#myGradient)" />'#13#10'</svg>'
      end>
    Left = 40
    Top = 400
  end
  object vilIndicators: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'StatusLED'
        Name = 'StatusLED'
      end
      item
        CollectionIndex = 1
        CollectionName = 'ExternalToolsLED'
        Name = 'ExternalToolsLED'
      end
      item
        CollectionIndex = 2
        CollectionName = 'LspLED'
        Name = 'LspLED'
      end>
    ImageCollection = icIndicators
    PreserveItems = True
    Width = 12
    Height = 12
    Left = 112
    Top = 400
  end
  object vilTabDecorators: TVirtualImageList
    Images = <
      item
        CollectionIndex = 9
        CollectionName = 'Bug'
        Name = 'Bug'
      end
      item
        CollectionIndex = 60
        CollectionName = 'Lock'
        Name = 'Lock'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 14
    Height = 14
    Left = 184
    Top = 400
  end
end
