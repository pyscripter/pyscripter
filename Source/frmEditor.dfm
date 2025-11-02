object EditorForm: TEditorForm
  Left = 304
  Top = 173
  BorderStyle = bsNone
  ClientHeight = 422
  ClientWidth = 582
  Color = clWindow
  Ctl3D = False
  ParentFont = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHelp = FormHelp
  TextHeight = 15
  object BGPanel: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 422
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    TabOrder = 0
    StyleElements = [seFont]
    object ViewsTabControl: TSpTBXTabControl
      Left = 2
      Top = 2
      Width = 578
      Height = 418
      Align = alClient
      OnContextPopup = ViewsTabControlContextPopup
      OnEnter = FGPanelEnter
      OnExit = FGPanelExit
      ActiveTabIndex = 0
      Images = PyIDEMainForm.vilImages
      OnActiveTabChange = ViewsTabControlActiveTabChange
      HiddenItems = <>
      object tabSource: TSpTBXTabItem
        Caption = 'Source'
        Checked = True
      end
      object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
        CustomWidth = 470
      end
      object tbiUpdateView: TSpTBXItem
        Caption = 'Update View'
        Hint = 'Update View|Update the selected view'
        ImageIndex = 29
        ImageName = 'Refresh'
        OnClick = mnUpdateViewClick
      end
      object tbiCloseTab: TSpTBXItem
        Caption = 'Close Tab'
        Hint = 'Close active tab'
        ImageIndex = 92
        ImageName = 'TabClose'
        OnClick = mnCloseTabClick
      end
      object tbshSource: TSpTBXTabSheet
        Left = 0
        Top = 30
        Width = 578
        Height = 388
        Caption = 'Source'
        ImageIndex = -1
        TabItem = 'tabSource'
        object SynEdit: TSynEdit
          Left = 2
          Top = 0
          Width = 367
          Height = 384
          HelpContext = 510
          Align = alClient
          Ctl3D = False
          ParentCtl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          ParentShowHint = False
          PopupMenu = pmnuEditor
          ShowHint = True
          TabOrder = 0
          OnDblClick = SynEditDblClick
          OnEnter = SynEditEnter
          OnExit = SynEditExit
          OnKeyDown = SynEditKeyDown
          OnKeyUp = SynEditKeyUp
          OnMouseDown = SynEditMouseDown
          UseCodeFolding = False
          BookMarkOptions.BookmarkImages = vilEditorMarks
          BorderStyle = bsNone
          Gutter.BorderStyle = gbsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Gradient = True
          Gutter.GradientSteps = 30
          Gutter.TrackChanges.Width = 2
          Gutter.TrackChanges.Visible = True
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 13
              OnCLick = SynEditGutterMarksCLick
              OnContextPopup = SynEditTSynGutterBands0ContextPopup
              OnMouseCursor = SynEditGutterMarksMouseCursor
            end
            item
              Kind = gbkCustom
              Width = 13
              OnPaintLines = SynEditDebugInfoPaintLines
              OnCLick = SynEditGutterDebugInfoCLick
              OnContextPopup = BreakpointContextPopup
              OnMouseCursor = SynEditGutterDebugInfoMouseCursor
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkTrackChanges
              Background = gbbEditor
            end
            item
              Kind = gbkMargin
              Width = 2
              Background = gbbEditor
            end>
          IndentGuides.Style = igsDotted
          ScrollbarAnnotations = <
            item
              AnnType = sbaCarets
              AnnPos = sbpFullWidth
              FullRow = False
            end
            item
              AnnType = sbaBookmark
              AnnPos = sbpLeft
              FullRow = True
            end
            item
              AnnType = sbaTrackChanges
              AnnPos = sbpRight
              FullRow = True
            end>
          SelectedColor.Background = clSkyBlue
          TabWidth = 4
          WantTabs = True
          OnChange = SynEditChange
          OnGutterGetText = SynEditGutterGetText
          OnMouseCursor = SynEditMouseCursor
          OnShowHint = EditorShowHint
          OnSpecialLineColors = SynEditSpecialLineColors
          OnStatusChange = SynEditStatusChange
        end
        object SynEdit2: TSynEdit
          Left = 374
          Top = 0
          Width = 200
          Height = 384
          HelpContext = 510
          Align = alRight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          ParentShowHint = False
          PopupMenu = pmnuEditor
          ShowHint = True
          TabOrder = 1
          Visible = False
          OnDblClick = SynEditDblClick
          OnEnter = SynEditEnter
          OnExit = SynEditExit
          OnKeyDown = SynEditKeyDown
          OnKeyUp = SynEditKeyUp
          OnMouseDown = SynEditMouseDown
          UseCodeFolding = False
          BookMarkOptions.BookmarkImages = vilEditorMarks
          BorderStyle = bsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 13
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkTrackChanges
              Background = gbbEditor
            end
            item
              Kind = gbkMargin
              Width = 2
              Background = gbbEditor
            end>
          ScrollbarAnnotations = <
            item
              AnnType = sbaCarets
              AnnPos = sbpFullWidth
              FullRow = False
            end
            item
              AnnType = sbaBookmark
              AnnPos = sbpLeft
              FullRow = True
            end
            item
              AnnType = sbaTrackChanges
              AnnPos = sbpRight
              FullRow = True
            end>
          OnMouseCursor = SynEditMouseCursor
          OnShowHint = EditorShowHint
          OnSpecialLineColors = SynEditSpecialLineColors
          OnStatusChange = SynEditStatusChange
        end
        object EditorSplitter: TSpTBXSplitter
          Left = 369
          Top = 0
          Height = 384
          Cursor = crSizeWE
          Align = alRight
          ParentColor = False
          Visible = False
          GripSize = 80
        end
      end
    end
  end
  object pmnuEditor: TSpTBXPopupMenu
    Images = PyIDEMainForm.vilImages
    Left = 96
    Top = 32
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
    object TBXSeparatorItem9: TSpTBXSeparatorItem
    end
    object mniAssistant: TSpTBXSubmenuItem
      Caption = 'Assistant'
      HelpContext = 750
      ImageIndex = 105
      ImageName = 'Assistant'
      LinkSubitems = CommandsDataModule.spiAssistant
    end
    object mnSourceCode: TSpTBXSubmenuItem
      Caption = 'Source Code'
      LinkSubitems = PyIDEMainForm.mnSourceCode
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnSearch: TSpTBXSubmenuItem
      Caption = 'Search'
      LinkSubitems = PyIDEMainForm.SearchMenu
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object SpTBXSubmenuItem1: TSpTBXSubmenuItem
      Caption = 'Spelling'
      ImageIndex = 102
      ImageName = 'SpellCheck'
      LinkSubitems = CommandsDataModule.mnSpelling
    end
    object SpTBXSeparatorItem8: TSpTBXSeparatorItem
    end
    object mnFoldVisible: TSpTBXItem
      Action = CommandsDataModule.actFoldVisible
    end
    object mnFold: TSpTBXSubmenuItem
      Caption = 'Fold'
      object mnFoldAll: TSpTBXItem
        Action = CommandsDataModule.actFoldAll
      end
      object mnFoldNearest: TSpTBXItem
        Action = CommandsDataModule.actFoldNearest
      end
      object mnFoldRegions: TSpTBXItem
        Action = CommandsDataModule.actFoldRegions
      end
      object SpTBXSeparatorItem3: TSpTBXSeparatorItem
      end
      object mnFoldLevel1: TSpTBXItem
        Action = CommandsDataModule.actFoldLevel1
      end
      object mnFoldLevel2: TSpTBXItem
        Action = CommandsDataModule.actFoldLevel2
      end
      object mnFoldLevel3: TSpTBXItem
        Action = CommandsDataModule.actFoldLevel3
      end
      object SpTBXSeparatorItem6: TSpTBXSeparatorItem
      end
      object mnFoldClasses: TSpTBXItem
        Action = CommandsDataModule.actFoldClasses
      end
      object mnFoldFunctions: TSpTBXItem
        Action = CommandsDataModule.actFoldFunctions
      end
    end
    object mnUnfold: TSpTBXSubmenuItem
      Caption = 'Unfold'
      object mnUnfoldAll: TSpTBXItem
        Action = CommandsDataModule.actUnfoldAll
      end
      object mnUnfoldNearest: TSpTBXItem
        Action = CommandsDataModule.actUnfoldNearest
      end
      object mnUnfoldRegions: TSpTBXItem
        Action = CommandsDataModule.actUnfoldRegions
      end
      object SpTBXSeparatorItem5: TSpTBXSeparatorItem
      end
      object mnUnfoldLevel1: TSpTBXItem
        Action = CommandsDataModule.actUnfoldLevel1
      end
      object mnUnfoldLevel2: TSpTBXItem
        Action = CommandsDataModule.actUnfoldLevel2
      end
      object mnUnfoldLevel3: TSpTBXItem
        Action = CommandsDataModule.actUnfoldLevel3
      end
      object SpTBXSeparatorItem7: TSpTBXSeparatorItem
      end
      object mnUnfoldClasses: TSpTBXItem
        Action = CommandsDataModule.actUnfoldClasses
      end
      object mnUnfoldFunctions: TSpTBXItem
        Action = CommandsDataModule.actUnfoldFunctions
      end
    end
    object SpTBXSeparatorItem4: TSpTBXSeparatorItem
    end
    object mnMaximizeEditor2: TSpTBXItem
      Action = PyIDEMainForm.actMaximizeEditor
    end
    object mnRestoreEditor2: TSpTBXItem
      Action = PyIDEMainForm.actRestoreEditor
    end
    object N12: TSpTBXSeparatorItem
    end
    object spiEditorViews: TSpTBXSubmenuItem
      Caption = 'Editor Views'
      LinkSubitems = PyIDEMainForm.EditorViewsMenu
    end
    object spiSeparator: TSpTBXSeparatorItem
    end
    object mnEditorOptions: TSpTBXItem
      Action = CommandsDataModule.actEditorOptions
    end
  end
  object pmnuViewsTab: TSpTBXPopupMenu
    Images = PyIDEMainForm.vilImages
    Left = 168
    Top = 32
    object mnUpdateView: TSpTBXItem
      Caption = 'Update View'
      Hint = 'Update View|Update the selected view'
      ImageIndex = 39
      ImageName = 'Breakpoint'
      OnClick = mnUpdateViewClick
    end
    object mnCloseTab: TSpTBXItem
      Caption = 'Close Tab'
      Hint = 'Close active tab'
      ImageIndex = 52
      ImageName = 'PageSetup'
      OnClick = mnCloseTabClick
    end
  end
  object vilGutterGlyphs: TVirtualImageList
    Images = <
      item
        CollectionIndex = 5
        CollectionName = 'EditorGutter\Executable'
        Name = 'Executable'
      end
      item
        CollectionIndex = 3
        CollectionName = 'EditorGutter\Current'
        Name = 'Current'
      end
      item
        CollectionIndex = 4
        CollectionName = 'EditorGutter\CurrentBreak'
        Name = 'CurrentBreak'
      end
      item
        CollectionIndex = 0
        CollectionName = 'EditorGutter\Break'
        Name = 'Break'
      end
      item
        CollectionIndex = 2
        CollectionName = 'EditorGutter\BreakInvalid'
        Name = 'BreakInvalid'
      end
      item
        CollectionIndex = 1
        CollectionName = 'EditorGutter\BreakDisabled'
        Name = 'BreakDisabled'
      end>
    ImageCollection = ResourcesDataModule.icGutterGlyphs
    PreserveItems = True
    Width = 11
    Height = 14
    Left = 168
    Top = 80
  end
  object vilCodeImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 8
        CollectionName = 'CodeImages\Python'
        Name = 'Python'
      end
      item
        CollectionIndex = 9
        CollectionName = 'CodeImages\Variable'
        Name = 'Variable'
      end
      item
        CollectionIndex = 1
        CollectionName = 'CodeImages\Field'
        Name = 'Field'
      end
      item
        CollectionIndex = 2
        CollectionName = 'CodeImages\Function'
        Name = 'Function'
      end
      item
        CollectionIndex = 5
        CollectionName = 'CodeImages\Method'
        Name = 'Method'
      end
      item
        CollectionIndex = 0
        CollectionName = 'CodeImages\Class'
        Name = 'Class'
      end
      item
        CollectionIndex = 7
        CollectionName = 'CodeImages\Namespace'
        Name = 'Namespace'
      end
      item
        CollectionIndex = 4
        CollectionName = 'CodeImages\List'
        Name = 'List'
      end
      item
        CollectionIndex = 6
        CollectionName = 'CodeImages\Module'
        Name = 'Module'
      end
      item
        CollectionIndex = 3
        CollectionName = 'CodeImages\Keyword'
        Name = 'Keyword'
      end>
    ImageCollection = ResourcesDataModule.icCodeImages
    PreserveItems = True
    Left = 92
    Top = 81
  end
  object pmnuBreakpoint: TSpTBXPopupMenu
    Left = 256
    Top = 32
    object spiBreakpointEnabled: TSpTBXItem
      Caption = 'Enabled'
      Hint = 'Enable/Disable breakpoint'
      AutoCheck = True
      OnClick = spiBreakpointEnabledClick
    end
    object spiBreakpointProperties: TSpTBXItem
      Caption = 'Breakpoint Properties...'
      Hint = 'Edit breakpoint properties'
      OnClick = spiBreakpointPropertiesClick
    end
    object spiSeparatorItem: TSpTBXSeparatorItem
    end
    object spiBreakpointClear: TSpTBXItem
      Caption = 'Clear'
      Hint = 'Clear breakpoint'
      OnClick = spiBreakpointClearClick
    end
  end
  object vilEditorMarks: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Mark0'
        Name = 'Mark0'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Mark1'
        Name = 'Mark1'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Mark2'
        Name = 'Mark2'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Mark3'
        Name = 'Mark3'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Mark4'
        Name = 'Mark4'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Mark5'
        Name = 'Mark5'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Mark6'
        Name = 'Mark6'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Mark7'
        Name = 'Mark7'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Mark8'
        Name = 'Mark8'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Mark9'
        Name = 'Mark9'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Bulb'
        Name = 'Bulb'
      end>
    ImageCollection = ResourcesDataModule.icEditorMarks
    PreserveItems = True
    Width = 11
    Height = 14
    Left = 257
    Top = 80
  end
  object pmnuDiagnostics: TSpTBXPopupMenu
    Images = PyIDEMainForm.vilImages
    Left = 96
    Top = 136
    object mnFixIssue: TSpTBXItem
      Caption = 'Quick Fix Issue'
      ImageIndex = 115
      ImageName = 'BugFix'
      OnClick = mnFixIssueClick
    end
    object mnIgnoreIssue: TSpTBXItem
      Caption = 'Ignore Issue'
      Hint = 'Flag the issue as ignored'
      OnClick = mnIgnoreIssueClick
    end
  end
  object pmnuRefactor: TSpTBXPopupMenu
    Images = PyIDEMainForm.vilImages
    OnPopup = pmnuRefactorPopup
    OnClosePopup = pmnuRefactorClosePopup
    Left = 184
    Top = 136
    object mnOrganizeImports: TSpTBXItem
      Action = CommandsDataModule.actOrganizeImports
    end
    object SpTBXSeparatorItem9: TSpTBXSeparatorItem
    end
    object mnRename: TSpTBXItem
      Action = CommandsDataModule.actRefactorRename
    end
  end
end
