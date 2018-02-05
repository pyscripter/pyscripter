object PyIDEMainForm: TPyIDEMainForm
  Left = 0
  Top = 115
  HelpContext = 100
  Caption = 'Python Scripter'
  ClientHeight = 538
  ClientWidth = 780
  Color = clWindow
  Ctl3D = False
  ParentFont = True
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 513
    Width = 780
    Height = 25
    object lbStatusMessage: TSpTBXLabelItem
      Wrapping = twNone
      CustomHeight = 21
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      CustomWidth = 0
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object lbStatusCaret: TSpTBXLabelItem
      Wrapping = twNone
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 84
    end
    object SpTBXSeparatorItem6: TSpTBXSeparatorItem
    end
    object lbStatusModified: TSpTBXLabelItem
      Wrapping = twNone
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 72
    end
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object lbStatusOverwrite: TSpTBXLabelItem
      Wrapping = twNone
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 84
    end
    object SpTBXSeparatorItem8: TSpTBXSeparatorItem
    end
    object lbStatusCaps: TSpTBXLabelItem
      Wrapping = twNone
      Options = [tboNoAutoHint]
      Alignment = taCenter
      CustomWidth = 40
    end
    object SpTBXSeparatorItem9: TSpTBXSeparatorItem
    end
    object tbciStatusLed: TTBControlItem
      Control = StatusLED
    end
    object SpTBXSeparatorItem10: TSpTBXSeparatorItem
    end
    object tbciStatusExternal: TTBControlItem
      Control = ExternalToolsLED
    end
    object StatusLED: TAMHLEDVecStd
      Left = 733
      Top = 4
      Width = 12
      Height = 12
      Hint = 'Ready'
      BorderStyle = bsNone
      ColorEffect = True
      ColorEffectOff = clSilver
      ColorEffectOn = clSilver
      ColorLeftTop = clSilver
      ColorRightBottom = clGray
      LEDActive = True
      LEDColorOff = clRed
      LEDColorOn = clGreen
      LEDStyle = lsEllipse
    end
    object ExternalToolsLED: TAMHLEDVecStd
      Left = 751
      Top = 4
      Width = 12
      Height = 12
      Hint = 'External Tool Running'
      BorderStyle = bsNone
      ColorEffect = True
      ColorEffectOff = clSilver
      ColorEffectOn = clSilver
      ColorLeftTop = clSilver
      ColorRightBottom = clGray
      LEDActive = False
      LEDColorOff = clRed
      LEDColorOn = clGreen
      LEDStyle = lsEllipse
      Visible = False
      PopupMenu = RunningProcessesPopUpMenu
    end
  end
  object BGPanel: TPanel
    Left = 9
    Top = 78
    Width = 762
    Height = 426
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    TabOrder = 2
    object TabControl1: TSpTBXTabControl
      Left = 0
      Top = 0
      Width = 758
      Height = 426
      Align = alClient
      PopupMenu = TabControlPopupMenu
      OnContextPopup = TabContolContextPopup
      ActiveTabIndex = -1
      Images = CommandsDataModule.Images
      TabDragReorder = True
      TabPosition = ttpBottom
      OnActiveTabChange = TabControlActiveTabChange
      HiddenItems = <>
      object tbiRightAlign: TSpTBXRightAlignSpacerItem
        CustomWidth = 648
      end
      object tbiTabSep: TSpTBXSeparatorItem
      end
      object tbiTabFiles: TSpTBXSubmenuItem
        Tag = 1
        Hint = 'Select File'
        ImageIndex = 151
        Options = [tboDropdownArrow]
        LinkSubitems = mnFiles
      end
      object tbiScrollLeft: TSpTBXItem
        Tag = 1
        Hint = 'Scroll left'
        Enabled = False
        ImageIndex = 80
        Options = [tboToolbarStyle]
        OnClick = tbiScrollLeftClick
      end
      object tbiScrollRight: TSpTBXItem
        Tag = 1
        Hint = 'Scroll right'
        Enabled = False
        ImageIndex = 79
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
      Left = 758
      Top = 0
      Width = 0
      Height = 426
      Align = alRight
      PopupMenu = TabControlPopupMenu
      Visible = False
      OnContextPopup = TabContolContextPopup
      ActiveTabIndex = -1
      Images = CommandsDataModule.Images
      TabDragReorder = True
      TabPosition = ttpBottom
      OnActiveTabChange = TabControlActiveTabChange
      HiddenItems = <>
      object SpTBXRightAlignSpacerItem2: TSpTBXRightAlignSpacerItem
      end
      object SpTBXSeparatorItem13: TSpTBXSeparatorItem
      end
      object tbiTabFiles2: TSpTBXSubmenuItem
        Tag = 2
        Hint = 'Select File'
        ImageIndex = 151
        Options = [tboDropdownArrow]
        LinkSubitems = mnFiles
      end
      object tbiScrollLeft2: TSpTBXItem
        Tag = 2
        Hint = 'Scroll left'
        Enabled = False
        ImageIndex = 80
        Options = [tboToolbarStyle]
        OnClick = tbiScrollLeftClick
      end
      object tbiScrollRight2: TSpTBXItem
        Tag = 2
        Hint = 'Scroll right'
        Enabled = False
        ImageIndex = 79
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
      Left = 758
      Top = 0
      Width = 4
      Height = 426
      Cursor = crSizeWE
      Align = alRight
      Color = clNone
      ParentColor = False
      Visible = False
    end
  end
  object TBXDockTop: TSpTBXDock
    Left = 0
    Top = 0
    Width = 780
    Height = 78
    PopupMenu = ToolbarPopupMenu
    object MainMenu: TSpTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      Images = CommandsDataModule.Images
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
          object mnEditUtf8: TSpTBXItem
            Action = CommandsDataModule.actEditUTF8
            RadioItem = True
          end
          object mnEditUtf8NoBom: TSpTBXItem
            Action = CommandsDataModule.actEditUTF8NoBOM
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
          Caption = '&IDE Windows'
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
          Caption = '&Navigate'
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
            OnClick = mnNoSyntaxClick
          end
        end
        object TBXSeparatorItem21: TSpTBXSeparatorItem
        end
        object mnLanguage: TSpTBXSubmenuItem
          Caption = 'Language'
        end
        object mnLayouts: TSpTBXSubmenuItem
          Caption = 'Layouts'
          ImageIndex = 108
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
            Caption = '&Maximize Editor'
            Hint = 'Maximize editor window'
            HelpContext = 270
            ImageIndex = 118
            ShortCut = 32858
            OnClick = actMaximizeEditorExecute
          end
          object TBXItem77: TSpTBXItem
            Caption = '&Restore Editor'
            Hint = 'Restore editor window'
            HelpContext = 270
            ImageIndex = 119
            ShortCut = 41050
            OnClick = actRestoreEditorExecute
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
        end
        object mnProjectOpen: TSpTBXItem
          Action = ProjectExplorerWindow.actProjectOpen
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
          object TBXSeparatorItem26: TSpTBXSeparatorItem
          end
          object mnReinitEngine: TSpTBXItem
            Action = actPythonReinitialize
          end
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
          ImageIndex = 82
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
        object N17: TSpTBXSeparatorItem
        end
        object mnHelpAbout: TSpTBXItem
          Action = CommandsDataModule.actAbout
        end
      end
    end
    object MainToolBar: TSpTBXToolbar
      Left = 0
      Top = 26
      DockPos = -6
      DockRow = 1
      Images = CommandsDataModule.Images
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
      Top = 26
      DockPos = 361
      DockRow = 1
      Images = CommandsDataModule.Images
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
      Left = 728
      Top = 26
      DockPos = 728
      DockRow = 1
      Images = CommandsDataModule.Images
      TabOrder = 4
      Caption = 'View Toolbar'
      object SpTBXItem4: TSpTBXItem
        Action = actSelectStyle
      end
      object tbiViewLayouts: TSpTBXSubmenuItem
        Caption = 'Layouts'
        Hint = 'Layouts'
        ImageIndex = 108
        Options = [tboDropdownArrow]
        LinkSubitems = mnLayouts
      end
    end
    object EditorToolbar: TSpTBXToolbar
      Left = 574
      Top = 26
      DockPos = 574
      DockRow = 1
      Images = CommandsDataModule.Images
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
      Left = 1
      Top = 52
      CloseButtonWhenDocked = True
      DockPos = 1
      DockRow = 2
      Images = CommandsDataModule.Images
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
        ImageIndex = 92
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
        Top = 0
        Width = 160
        Height = 21
        AutoCloseUp = True
        ItemHeight = 13
        TabOrder = 0
        OnChange = tbiSearchTextChange
        OnExit = tbiSearchTextExit
        OnKeyPress = tbiSearchTextKeyPress
        AutoDropDownWidth = True
      end
      object tbiReplaceText: TSpTBXComboBox
        Left = 265
        Top = 0
        Width = 160
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Visible = False
        OnChange = tbiReplaceTextChange
        OnKeyPress = tbiReplaceTextKeyPress
        AutoDropDownWidth = True
      end
    end
    object UserToolbar: TSpTBXToolbar
      Left = 496
      Top = 52
      DockPos = 310
      DockRow = 2
      Images = CommandsDataModule.Images
      TabOrder = 6
      Visible = False
      Caption = 'User Toolbar'
    end
  end
  object TBXDockLeft: TSpTBXDock
    Left = 0
    Top = 78
    Width = 9
    Height = 426
    FixAlign = True
    PopupMenu = ToolbarPopupMenu
    Position = dpLeft
  end
  object TBXDockRight: TSpTBXDock
    Left = 771
    Top = 78
    Width = 9
    Height = 426
    FixAlign = True
    PopupMenu = ToolbarPopupMenu
    Position = dpRight
  end
  object TBXDockBottom: TSpTBXDock
    Left = 0
    Top = 504
    Width = 780
    Height = 9
    FixAlign = True
    PopupMenu = ToolbarPopupMenu
    Position = dpBottom
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
    DockStyle = JvDockVSNetStyleSpTBX
    Left = 37
    Top = 158
  end
  object AppStorage: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.DateTimeAsString = False
    StorageOptions.DefaultIfReadConvertError = True
    FlushOnDestroy = False
    Location = flCustom
    DefaultSection = 'Other Settings'
    SubStorages = <>
    Left = 394
    Top = 93
  end
  object CloseTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = CloseTimerTimer
    Left = 39
    Top = 219
  end
  object TabControlPopupMenu: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 312
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
    Left = 172
    Top = 280
  end
  object JvAppInstances: TJvAppInstances
    Active = False
    OnCmdLineReceived = JvAppInstancesCmdLineReceived
    Left = 467
    Top = 93
  end
  object SpTBXCustomizer: TSpTBXCustomizer
    Images = CommandsDataModule.Images
    OnGetCustomizeForm = SpTBXCustomizerGetCustomizeForm
    Left = 280
    Top = 96
  end
  object ToolbarPopupMenu: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    LinkSubitems = mnuToolbars
    Left = 40
    Top = 280
  end
  object JvFormStorage: TJvFormStorage
    Active = False
    AppStorage = AppStorage
    AppStoragePath = 'Main Form Placement\'
    VersionCheck = fpvcNocheck
    StoredValues = <>
    Left = 545
    Top = 93
  end
  object actlImmutable: TActionList
    Images = CommandsDataModule.Images
    Left = 112
    Top = 100
    object actViewNextEditor: TAction
      Category = 'View'
      Caption = '&Next Editor'
      HelpContext = 360
      Hint = 'Next Editor|Move to the next editor'
      ImageIndex = 79
      ShortCut = 16393
      OnExecute = actNextEditorExecute
    end
    object actViewPreviousEditor: TAction
      Category = 'View'
      Caption = '&Previous Editor'
      HelpContext = 360
      Hint = 'Previous editor|Move to the previous editor'
      ImageIndex = 80
      ShortCut = 24585
      OnExecute = actPreviousEditorExecute
    end
  end
  object actlStandard: TActionList
    Images = CommandsDataModule.Images
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
      ImageIndex = 63
      OnExecute = actCallStackWinExecute
    end
    object actVariablesWin: TAction
      Category = 'Debug Windows'
      Caption = '&Variables'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Variables window'
      ImageIndex = 65
      OnExecute = actVariablesWinExecute
    end
    object actSyntaxCheck: TAction
      Category = 'Run'
      Caption = '&Syntax Check'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Syntax Check|Perform syntax check and load scripts'
      ImageIndex = 20
      OnExecute = actSyntaxCheckExecute
    end
    object actRun: TAction
      Category = 'Run'
      Caption = '&Run'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Run|Run active module'
      ImageIndex = 51
      ShortCut = 16504
      OnExecute = actRunExecute
    end
    object actCommandLine: TAction
      Category = 'Run'
      Caption = 'Command Line &Parameters...'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Set command line parameters'
      ImageIndex = 23
      OnExecute = actCommandLineExecute
    end
    object actImportModule: TAction
      Category = 'Run'
      Caption = '&Import Module'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Import module'
      ImageIndex = 34
      OnExecute = actImportModuleExecute
    end
    object actFileNewModule: TAction
      Category = 'File'
      Caption = '&New Python module'
      HelpContext = 310
      HelpType = htContext
      Hint = 'New Python module'
      ImageIndex = 0
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
      ImageIndex = 150
      ShortCut = 24691
      OnExecute = actFileCloseAllExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      HelpContext = 310
      HelpType = htContext
      Hint = 'Exit'
      ImageIndex = 44
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actViewStatusBar: TAction
      Category = 'View'
      Caption = '&Status Bar'
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
      ImageIndex = 27
      ShortCut = 32888
      OnExecute = actExternalRunExecute
    end
    object actExternalRunConfigure: TAction
      Category = 'Run'
      Caption = '&Configure External Run...'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Configure External Run'
      ImageIndex = 88
      OnExecute = actExternalRunConfigureExecute
    end
    object actRunDebugLastScript: TAction
      Category = 'Run'
      Caption = 'Debug Last Script'
      HelpType = htContext
      Hint = 'Debug last script'
      ImageIndex = 145
      ShortCut = 8312
      OnExecute = actRunDebugLastScriptExecute
    end
    object actDebug: TAction
      Category = 'Run'
      Caption = '&Debug'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Debug|Debug active script'
      ImageIndex = 53
      ShortCut = 120
      OnExecute = actDebugExecute
    end
    object actRestoreEditor: TAction
      Category = 'View'
      Caption = '&Restore Editor'
      HelpContext = 270
      HelpType = htContext
      Hint = 'Restore editor window'
      ImageIndex = 119
      ShortCut = 41050
      OnExecute = actRestoreEditorExecute
    end
    object actRunToCursor: TAction
      Category = 'Run'
      Caption = 'Run To &Cursor'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Run to cursor'
      ImageIndex = 54
      ShortCut = 115
      OnExecute = actRunToCursorExecute
    end
    object actStepInto: TAction
      Category = 'Run'
      Caption = 'Step &Into'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Step into subroutine '
      ImageIndex = 56
      ShortCut = 118
      OnExecute = actStepIntoExecute
    end
    object actStepOver: TAction
      Category = 'Run'
      Caption = 'Step &Over'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Step over next function call'
      ImageIndex = 57
      ShortCut = 119
      OnExecute = actStepOverExecute
    end
    object actStepOut: TAction
      Category = 'Run'
      Caption = 'Step O&ut'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Step out of the current subroutine'
      ImageIndex = 58
      ShortCut = 8311
      OnExecute = actStepOutExecute
    end
    object actDebugPause: TAction
      Category = 'Run'
      Caption = '&Pause'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Pause running program'
      ImageIndex = 116
      OnExecute = actDebugPauseExecute
    end
    object actDebugAbort: TAction
      Category = 'Run'
      Caption = '&Abort Debugging'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Abort debugging'
      ImageIndex = 59
      ShortCut = 49272
      OnExecute = actDebugAbortExecute
    end
    object actRunLastScriptExternal: TAction
      Category = 'Run'
      Caption = 'Run Last Script Externally'
      HelpType = htContext
      Hint = 'Run last script externally'
      ImageIndex = 146
      ShortCut = 41080
      OnExecute = actRunLastScriptExternalExecute
    end
    object actRunLastScript: TAction
      Category = 'Run'
      Caption = 'Run Last Script'
      HelpType = htContext
      Hint = 'Run last script'
      ImageIndex = 144
      ShortCut = 24696
      OnExecute = actRunLastScriptExecute
    end
    object actToggleBreakPoint: TAction
      Category = 'Run'
      Caption = 'Toggle &breakpoint'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Toggle breakpoint'
      ImageIndex = 60
      ShortCut = 116
      OnExecute = actToggleBreakPointExecute
    end
    object actClearAllBreakpoints: TAction
      Category = 'Run'
      Caption = '&Clear All Breakpoints'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Clear all breakpoints'
      ImageIndex = 61
      OnExecute = actClearAllBreakpointsExecute
    end
    object actBreakPointsWin: TAction
      Category = 'Debug Windows'
      Caption = '&Breakpoints'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Breakpoints window'
      ImageIndex = 67
      OnExecute = actBreakPointsWinExecute
    end
    object actWatchesWin: TAction
      Category = 'Debug Windows'
      Caption = '&Watches'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Watches window'
      ImageIndex = 66
      OnExecute = actWatchesWinExecute
    end
    object actMessagesWin: TAction
      Category = 'Debug Windows'
      Caption = '&Messages'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'Show/Hide Messages window'
      ImageIndex = 75
      OnExecute = actMessagesWinExecute
    end
    object actViewII: TAction
      Category = 'View'
      Caption = '&Interactive Interpreter'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Interactive Interpreter'
      ImageIndex = 128
      OnExecute = actViewIIExecute
    end
    object actViewCodeExplorer: TAction
      Category = 'View'
      Caption = '&Code Explorer'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Code Explorer'
      ImageIndex = 76
      OnExecute = actViewCodeExplorerExecute
    end
    object actViewFileExplorer: TAction
      Category = 'View'
      Caption = '&File Explorer'
      Checked = True
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide File Explorer'
      ImageIndex = 84
      OnExecute = actViewFileExplorerExecute
    end
    object actViewToDoList: TAction
      Category = 'View'
      Caption = '&To-Do List'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide To Do List'
      ImageIndex = 85
      OnExecute = actViewToDoListExecute
    end
    object actViewFindResults: TAction
      Category = 'View'
      Caption = '&Find in Files Results'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Find in Files Results'
      ImageIndex = 87
      ShortCut = 49222
      OnExecute = actViewFindResultsExecute
    end
    object actViewOutput: TAction
      Category = 'View'
      Caption = '&Output Window'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Output Window'
      ImageIndex = 89
      OnExecute = actViewOutputExecute
    end
    object actViewUnitTests: TAction
      Category = 'View'
      Caption = '&Unit Tests'
      HelpType = htContext
      Hint = 'View/Hide Unit Tests Window'
      ImageIndex = 103
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
      Caption = 'Find &References'
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
      ImageIndex = 96
      ShortCut = 32805
      OnExecute = tbiBrowsePreviousClick
    end
    object actBrowseForward: TAction
      Category = 'Refactoring'
      Caption = 'Browse &Forward'
      HelpContext = 830
      HelpType = htContext
      Hint = 'Browse forward'
      ImageIndex = 97
      ShortCut = 32807
      OnExecute = tbiBrowseNextClick
    end
    object actViewRegExpTester: TAction
      Category = 'View'
      Caption = '&Regular Expression Tester'
      HelpContext = 360
      HelpType = htContext
      Hint = 'View/Hide Regular Expression Tester'
      ImageIndex = 101
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
      ImageIndex = 118
      ShortCut = 32858
      OnExecute = actMaximizeEditorExecute
    end
    object actEditorZoomIn: TAction
      Category = 'View'
      Caption = 'Zoom &In'
      HelpType = htContext
      Hint = 'Increase the font size of the editor'
      ImageIndex = 112
      ShortCut = 32875
      OnExecute = actEditorZoomInExecute
    end
    object actEditorZoomOut: TAction
      Category = 'View'
      Caption = 'Zoom &Out'
      HelpType = htContext
      Hint = 'Decrease the font size of the editor'
      ImageIndex = 113
      ShortCut = 32877
      OnExecute = actEditorZoomOutExecute
    end
    object actViewSplitEditorVer: TAction
      Category = 'View'
      Caption = 'Split Editor Vertically'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Split the editor Windows vertically'
      ImageIndex = 125
      OnExecute = actViewSplitEditorVerExecute
    end
    object actAddWatchAtCursor: TAction
      Category = 'Run'
      Caption = 'Add &Watch At Cursor'
      HelpContext = 340
      HelpType = htContext
      Hint = 'Add the expression at the editor current position as a watch'
      ImageIndex = 66
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
      OnExecute = actNewFileExecute
    end
    object actNavWatches: TAction
      Category = 'IDE Navigation'
      Caption = '&Watches'
      HelpType = htContext
      Hint = 'Activate the Watches window'
      ImageIndex = 66
      ShortCut = 49239
      OnExecute = actNavWatchesExecute
    end
    object actNavBreakpoints: TAction
      Category = 'IDE Navigation'
      Caption = '&BreakPoints'
      HelpType = htContext
      Hint = 'Activate the Breakpoints window'
      ImageIndex = 67
      ShortCut = 49218
      OnExecute = actNavBreakpointsExecute
    end
    object actNavInterpreter: TAction
      Category = 'IDE Navigation'
      Caption = '&Interpreter'
      HelpType = htContext
      Hint = 'Activate the Interpreter window'
      ImageIndex = 128
      ShortCut = 49225
      OnExecute = actNavInterpreterExecute
    end
    object actNavVariables: TAction
      Category = 'IDE Navigation'
      Caption = '&Variables'
      HelpType = htContext
      Hint = 'Activate the Variables window'
      ImageIndex = 65
      ShortCut = 49238
      OnExecute = actNavVariablesExecute
    end
    object actNavCallStack: TAction
      Category = 'IDE Navigation'
      Caption = '&Call Stack'
      HelpType = htContext
      Hint = 'Activate the Call Stack window'
      ImageIndex = 63
      ShortCut = 49235
      OnExecute = actNavCallStackExecute
    end
    object actNavMessages: TAction
      Category = 'IDE Navigation'
      Caption = '&Messages '
      HelpType = htContext
      Hint = 'Activate the Messages window'
      ImageIndex = 75
      ShortCut = 49229
      OnExecute = actNavMessagesExecute
    end
    object actNavFileExplorer: TAction
      Category = 'IDE Navigation'
      Caption = 'File E&xplorer'
      HelpType = htContext
      Hint = 'Activate the File Explorer window'
      ImageIndex = 84
      ShortCut = 49240
      OnExecute = actNavFileExplorerExecute
    end
    object actNavCodeExplorer: TAction
      Category = 'IDE Navigation'
      Caption = '&Code Explorer'
      HelpType = htContext
      Hint = 'Activate the Code Explorer window'
      ImageIndex = 76
      ShortCut = 49219
      OnExecute = actNavCodeExplorerExecute
    end
    object actNavTodo: TAction
      Category = 'IDE Navigation'
      Caption = '&Todo List'
      HelpType = htContext
      Hint = 'Activate the Todo List window'
      ImageIndex = 85
      ShortCut = 49236
      OnExecute = actNavTodoExecute
    end
    object actNavUnitTests: TAction
      Category = 'IDE Navigation'
      Caption = '&Unit Tests'
      HelpType = htContext
      Hint = 'Activate the Todo List window'
      ImageIndex = 103
      ShortCut = 49237
      OnExecute = actNavUnitTestsExecute
    end
    object actNavOutput: TAction
      Category = 'IDE Navigation'
      Caption = 'Command &Output'
      HelpType = htContext
      Hint = 'Activate the Command Output window'
      ImageIndex = 89
      ShortCut = 49231
      OnExecute = actNavOutputExecute
    end
    object actNavEditor: TAction
      Category = 'IDE Navigation'
      Caption = '&Editor'
      HelpType = htContext
      Hint = 'Activate the Editor'
      ImageIndex = 117
      ShortCut = 123
      OnExecute = actNavEditorExecute
    end
    object actExecSelection: TAction
      Category = 'Run'
      Caption = 'E&xecute selection'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Execute the editor selection'
      ImageIndex = 19
      ShortCut = 16502
      OnExecute = actExecSelectionExecute
    end
    object actViewSplitEditorHor: TAction
      Category = 'View'
      Caption = 'Split Editor Horizontally'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Split the editor Windows horizontally'
      ImageIndex = 126
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
      ImageIndex = 127
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
      ImageIndex = 131
      OnExecute = actViewProjectExplorerExecute
    end
    object actNavProjectExplorer: TAction
      Category = 'IDE Navigation'
      Caption = '&Project Explorer'
      HelpContext = 360
      HelpType = htContext
      Hint = 'Activate the Project Explorer window'
      ImageIndex = 131
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
      ImageIndex = 125
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
      ImageIndex = 126
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
      Caption = 'Select Style'
      Hint = 'Select Style'
      ImageIndex = 107
      OnExecute = actSelectStyleExecute
    end
  end
  object JvDockVSNetStyleSpTBX: TJvDockVSNetStyleSpTBX
    AlwaysShowGrabber = False
    TabServerOption.HotTrack = True
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'Tahoma'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = 5395794
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'Tahoma'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.ShowCloseButtonOnTabs = False
    Left = 136
    Top = 158
  end
end
