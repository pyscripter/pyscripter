object EditorForm: TEditorForm
  Left = 304
  Top = 173
  BorderStyle = bsNone
  ClientHeight = 422
  ClientWidth = 582
  Color = clWindow
  Ctl3D = False
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
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
        CustomWidth = 478
      end
      object tbiUpdateView: TSpTBXItem
        Caption = 'Update View'
        Hint = 'Update View|Update the selected view'
        ImageIndex = 39
        OnClick = mnUpdateViewClick
      end
      object tbiCloseTab: TSpTBXItem
        Caption = 'Close Tab'
        Hint = 'Close active tab'
        ImageIndex = 52
        OnClick = mnCloseTabClick
      end
      object tbshSource: TSpTBXTabSheet
        Left = 0
        Top = 26
        Width = 578
        Height = 392
        Caption = 'Source'
        ImageIndex = -1
        TabItem = 'tabSource'
        object SynEdit: TSynEdit
          Left = 2
          Top = 0
          Width = 367
          Height = 388
          HelpContext = 510
          Align = alClient
          Ctl3D = False
          ParentCtl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          PopupMenu = pmnuEditor
          TabOrder = 0
          OnDblClick = SynEditDblClick
          OnEnter = SynEditEnter
          OnExit = SynEditExit
          OnKeyDown = SynEditKeyDown
          OnKeyUp = SynEditKeyUp
          OnMouseDown = SynEditMouseDown
          OnMouseMove = SynEditMouseMove
          OnMouseWheelDown = SynEditMouseWheelDown
          OnMouseWheelUp = SynEditMouseWheelUp
          UseCodeFolding = False
          BorderStyle = bsNone
          Gutter.BorderStyle = gbsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Gradient = True
          Gutter.GradientSteps = 30
          SelectedColor.Background = clSkyBlue
          TabWidth = 4
          WantTabs = True
          OnChange = SynEditChange
          OnGutterClick = SynEditGutterClick
          OnGutterGetText = SynEditGutterGetText
          OnMouseCursor = SynEditMouseCursor
          OnSpecialLineColors = SynEditSpecialLineColors
          OnStatusChange = SynEditStatusChange
          OnPaintTransient = SynEditPaintTransient
          RemovedKeystrokes = <
            item
              Command = ecUp
              ShortCut = 38
            end
            item
              Command = ecSelUp
              ShortCut = 8230
            end
            item
              Command = ecScrollUp
              ShortCut = 16422
            end
            item
              Command = ecDown
              ShortCut = 40
            end
            item
              Command = ecSelDown
              ShortCut = 8232
            end
            item
              Command = ecScrollDown
              ShortCut = 16424
            end
            item
              Command = ecLeft
              ShortCut = 37
            end
            item
              Command = ecSelLeft
              ShortCut = 8229
            end
            item
              Command = ecWordLeft
              ShortCut = 16421
            end
            item
              Command = ecSelWordLeft
              ShortCut = 24613
            end
            item
              Command = ecRight
              ShortCut = 39
            end
            item
              Command = ecSelRight
              ShortCut = 8231
            end
            item
              Command = ecWordRight
              ShortCut = 16423
            end
            item
              Command = ecSelWordRight
              ShortCut = 24615
            end
            item
              Command = ecPageDown
              ShortCut = 34
            end
            item
              Command = ecSelPageDown
              ShortCut = 8226
            end
            item
              Command = ecPageBottom
              ShortCut = 16418
            end
            item
              Command = ecSelPageBottom
              ShortCut = 24610
            end
            item
              Command = ecPageUp
              ShortCut = 33
            end
            item
              Command = ecSelPageUp
              ShortCut = 8225
            end
            item
              Command = ecPageTop
              ShortCut = 16417
            end
            item
              Command = ecSelPageTop
              ShortCut = 24609
            end
            item
              Command = ecLineStart
              ShortCut = 36
            end
            item
              Command = ecSelLineStart
              ShortCut = 8228
            end
            item
              Command = ecEditorTop
              ShortCut = 16420
            end
            item
              Command = ecSelEditorTop
              ShortCut = 24612
            end
            item
              Command = ecLineEnd
              ShortCut = 35
            end
            item
              Command = ecSelLineEnd
              ShortCut = 8227
            end
            item
              Command = ecEditorBottom
              ShortCut = 16419
            end
            item
              Command = ecSelEditorBottom
              ShortCut = 24611
            end
            item
              Command = ecToggleMode
              ShortCut = 45
            end
            item
              Command = ecCopy
              ShortCut = 16429
            end
            item
              Command = ecCut
              ShortCut = 8238
            end
            item
              Command = ecPaste
              ShortCut = 8237
            end
            item
              Command = ecDeleteChar
              ShortCut = 46
            end
            item
              Command = ecDeleteLastChar
              ShortCut = 8
            end
            item
              Command = ecDeleteLastChar
              ShortCut = 8200
            end
            item
              Command = ecDeleteLastWord
              ShortCut = 16392
            end
            item
              Command = ecUndo
              ShortCut = 32776
            end
            item
              Command = ecRedo
              ShortCut = 40968
            end
            item
              Command = ecLineBreak
              ShortCut = 13
            end
            item
              Command = ecLineBreak
              ShortCut = 8205
            end
            item
              Command = ecTab
              ShortCut = 9
            end
            item
              Command = ecShiftTab
              ShortCut = 8201
            end
            item
              Command = ecContextHelp
              ShortCut = 112
            end
            item
              Command = ecSelectAll
              ShortCut = 16449
            end
            item
              Command = ecCopy
              ShortCut = 16451
            end
            item
              Command = ecPaste
              ShortCut = 16470
            end
            item
              Command = ecCut
              ShortCut = 16472
            end
            item
              Command = ecBlockIndent
              ShortCut = 24649
            end
            item
              Command = ecBlockUnindent
              ShortCut = 24661
            end
            item
              Command = ecLineBreak
              ShortCut = 16461
            end
            item
              Command = ecInsertLine
              ShortCut = 16462
            end
            item
              Command = ecDeleteWord
              ShortCut = 16468
            end
            item
              Command = ecDeleteLine
              ShortCut = 16473
            end
            item
              Command = ecDeleteEOL
              ShortCut = 24665
            end
            item
              Command = ecUndo
              ShortCut = 16474
            end
            item
              Command = ecRedo
              ShortCut = 24666
            end
            item
              Command = ecGotoMarker0
              ShortCut = 16432
            end
            item
              Command = ecGotoMarker1
              ShortCut = 16433
            end
            item
              Command = ecGotoMarker2
              ShortCut = 16434
            end
            item
              Command = ecGotoMarker3
              ShortCut = 16435
            end
            item
              Command = ecGotoMarker4
              ShortCut = 16436
            end
            item
              Command = ecGotoMarker5
              ShortCut = 16437
            end
            item
              Command = ecGotoMarker6
              ShortCut = 16438
            end
            item
              Command = ecGotoMarker7
              ShortCut = 16439
            end
            item
              Command = ecGotoMarker8
              ShortCut = 16440
            end
            item
              Command = ecGotoMarker9
              ShortCut = 16441
            end
            item
              Command = ecSetMarker0
              ShortCut = 24624
            end
            item
              Command = ecSetMarker1
              ShortCut = 24625
            end
            item
              Command = ecSetMarker2
              ShortCut = 24626
            end
            item
              Command = ecSetMarker3
              ShortCut = 24627
            end
            item
              Command = ecSetMarker4
              ShortCut = 24628
            end
            item
              Command = ecSetMarker5
              ShortCut = 24629
            end
            item
              Command = ecSetMarker6
              ShortCut = 24630
            end
            item
              Command = ecSetMarker7
              ShortCut = 24631
            end
            item
              Command = ecSetMarker8
              ShortCut = 24632
            end
            item
              Command = ecSetMarker9
              ShortCut = 24633
            end
            item
              Command = ecNormalSelect
              ShortCut = 24654
            end
            item
              Command = ecColumnSelect
              ShortCut = 24643
            end
            item
              Command = ecLineSelect
              ShortCut = 24652
            end
            item
              Command = ecMatchBracket
              ShortCut = 24642
            end
            item
              Command = ecFoldAll
              ShortCut = 24765
            end
            item
              Command = ecUnfoldAll
              ShortCut = 24763
            end
            item
              Command = ecFoldNearest
              ShortCut = 16575
            end
            item
              Command = ecUnfoldNearest
              ShortCut = 24767
            end
            item
              Command = ecFoldLevel1
              ShortCut = 16459
              ShortCut2 = 16433
            end
            item
              Command = ecFoldLevel2
              ShortCut = 16459
              ShortCut2 = 16434
            end
            item
              Command = ecFoldLevel3
              ShortCut = 16459
              ShortCut2 = 16435
            end
            item
              Command = ecUnfoldLevel1
              ShortCut = 24651
              ShortCut2 = 24625
            end
            item
              Command = ecUnfoldLevel2
              ShortCut = 24651
              ShortCut2 = 24626
            end
            item
              Command = ecUnfoldLevel3
              ShortCut = 24651
              ShortCut2 = 24627
            end>
          AddedKeystrokes = <>
        end
        object SynEdit2: TSynEdit
          Left = 374
          Top = 0
          Width = 200
          Height = 388
          Align = alRight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          PopupMenu = pmnuEditor
          TabOrder = 1
          Visible = False
          OnEnter = SynEditEnter
          OnExit = SynEditExit
          OnKeyDown = SynEditKeyDown
          OnKeyUp = SynEditKeyUp
          OnMouseDown = SynEditMouseDown
          OnMouseMove = SynEditMouseMove
          OnMouseWheelDown = SynEditMouseWheelDown
          OnMouseWheelUp = SynEditMouseWheelUp
          UseCodeFolding = False
          BorderStyle = bsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          OnMouseCursor = SynEditMouseCursor
          OnSpecialLineColors = SynEditSpecialLineColors
          OnStatusChange = SynEditStatusChange
          OnPaintTransient = SynEditPaintTransient
          RemovedKeystrokes = <
            item
              Command = ecUp
              ShortCut = 38
            end
            item
              Command = ecSelUp
              ShortCut = 8230
            end
            item
              Command = ecScrollUp
              ShortCut = 16422
            end
            item
              Command = ecDown
              ShortCut = 40
            end
            item
              Command = ecSelDown
              ShortCut = 8232
            end
            item
              Command = ecScrollDown
              ShortCut = 16424
            end
            item
              Command = ecLeft
              ShortCut = 37
            end
            item
              Command = ecSelLeft
              ShortCut = 8229
            end
            item
              Command = ecWordLeft
              ShortCut = 16421
            end
            item
              Command = ecSelWordLeft
              ShortCut = 24613
            end
            item
              Command = ecRight
              ShortCut = 39
            end
            item
              Command = ecSelRight
              ShortCut = 8231
            end
            item
              Command = ecWordRight
              ShortCut = 16423
            end
            item
              Command = ecSelWordRight
              ShortCut = 24615
            end
            item
              Command = ecPageDown
              ShortCut = 34
            end
            item
              Command = ecSelPageDown
              ShortCut = 8226
            end
            item
              Command = ecPageBottom
              ShortCut = 16418
            end
            item
              Command = ecSelPageBottom
              ShortCut = 24610
            end
            item
              Command = ecPageUp
              ShortCut = 33
            end
            item
              Command = ecSelPageUp
              ShortCut = 8225
            end
            item
              Command = ecPageTop
              ShortCut = 16417
            end
            item
              Command = ecSelPageTop
              ShortCut = 24609
            end
            item
              Command = ecLineStart
              ShortCut = 36
            end
            item
              Command = ecSelLineStart
              ShortCut = 8228
            end
            item
              Command = ecEditorTop
              ShortCut = 16420
            end
            item
              Command = ecSelEditorTop
              ShortCut = 24612
            end
            item
              Command = ecLineEnd
              ShortCut = 35
            end
            item
              Command = ecSelLineEnd
              ShortCut = 8227
            end
            item
              Command = ecEditorBottom
              ShortCut = 16419
            end
            item
              Command = ecSelEditorBottom
              ShortCut = 24611
            end
            item
              Command = ecToggleMode
              ShortCut = 45
            end
            item
              Command = ecCopy
              ShortCut = 16429
            end
            item
              Command = ecCut
              ShortCut = 8238
            end
            item
              Command = ecPaste
              ShortCut = 8237
            end
            item
              Command = ecDeleteChar
              ShortCut = 46
            end
            item
              Command = ecDeleteLastChar
              ShortCut = 8
            end
            item
              Command = ecDeleteLastChar
              ShortCut = 8200
            end
            item
              Command = ecDeleteLastWord
              ShortCut = 16392
            end
            item
              Command = ecUndo
              ShortCut = 32776
            end
            item
              Command = ecRedo
              ShortCut = 40968
            end
            item
              Command = ecLineBreak
              ShortCut = 13
            end
            item
              Command = ecLineBreak
              ShortCut = 8205
            end
            item
              Command = ecTab
              ShortCut = 9
            end
            item
              Command = ecShiftTab
              ShortCut = 8201
            end
            item
              Command = ecContextHelp
              ShortCut = 112
            end
            item
              Command = ecSelectAll
              ShortCut = 16449
            end
            item
              Command = ecCopy
              ShortCut = 16451
            end
            item
              Command = ecPaste
              ShortCut = 16470
            end
            item
              Command = ecCut
              ShortCut = 16472
            end
            item
              Command = ecBlockIndent
              ShortCut = 24649
            end
            item
              Command = ecBlockUnindent
              ShortCut = 24661
            end
            item
              Command = ecLineBreak
              ShortCut = 16461
            end
            item
              Command = ecInsertLine
              ShortCut = 16462
            end
            item
              Command = ecDeleteWord
              ShortCut = 16468
            end
            item
              Command = ecDeleteLine
              ShortCut = 16473
            end
            item
              Command = ecDeleteEOL
              ShortCut = 24665
            end
            item
              Command = ecUndo
              ShortCut = 16474
            end
            item
              Command = ecRedo
              ShortCut = 24666
            end
            item
              Command = ecGotoMarker0
              ShortCut = 16432
            end
            item
              Command = ecGotoMarker1
              ShortCut = 16433
            end
            item
              Command = ecGotoMarker2
              ShortCut = 16434
            end
            item
              Command = ecGotoMarker3
              ShortCut = 16435
            end
            item
              Command = ecGotoMarker4
              ShortCut = 16436
            end
            item
              Command = ecGotoMarker5
              ShortCut = 16437
            end
            item
              Command = ecGotoMarker6
              ShortCut = 16438
            end
            item
              Command = ecGotoMarker7
              ShortCut = 16439
            end
            item
              Command = ecGotoMarker8
              ShortCut = 16440
            end
            item
              Command = ecGotoMarker9
              ShortCut = 16441
            end
            item
              Command = ecSetMarker0
              ShortCut = 24624
            end
            item
              Command = ecSetMarker1
              ShortCut = 24625
            end
            item
              Command = ecSetMarker2
              ShortCut = 24626
            end
            item
              Command = ecSetMarker3
              ShortCut = 24627
            end
            item
              Command = ecSetMarker4
              ShortCut = 24628
            end
            item
              Command = ecSetMarker5
              ShortCut = 24629
            end
            item
              Command = ecSetMarker6
              ShortCut = 24630
            end
            item
              Command = ecSetMarker7
              ShortCut = 24631
            end
            item
              Command = ecSetMarker8
              ShortCut = 24632
            end
            item
              Command = ecSetMarker9
              ShortCut = 24633
            end
            item
              Command = ecNormalSelect
              ShortCut = 24654
            end
            item
              Command = ecColumnSelect
              ShortCut = 24643
            end
            item
              Command = ecLineSelect
              ShortCut = 24652
            end
            item
              Command = ecMatchBracket
              ShortCut = 24642
            end
            item
              Command = ecFoldAll
              ShortCut = 24765
            end
            item
              Command = ecUnfoldAll
              ShortCut = 24763
            end
            item
              Command = ecFoldNearest
              ShortCut = 16575
            end
            item
              Command = ecUnfoldNearest
              ShortCut = 24767
            end
            item
              Command = ecFoldLevel1
              ShortCut = 16459
              ShortCut2 = 16433
            end
            item
              Command = ecFoldLevel2
              ShortCut = 16459
              ShortCut2 = 16434
            end
            item
              Command = ecFoldLevel3
              ShortCut = 16459
              ShortCut2 = 16435
            end
            item
              Command = ecUnfoldLevel1
              ShortCut = 24651
              ShortCut2 = 24625
            end
            item
              Command = ecUnfoldLevel2
              ShortCut = 24651
              ShortCut2 = 24626
            end
            item
              Command = ecUnfoldLevel3
              ShortCut = 24651
              ShortCut2 = 24627
            end>
          AddedKeystrokes = <>
        end
        object EditorSplitter: TSpTBXSplitter
          Left = 369
          Top = 0
          Height = 388
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
    object mnEditDelete: TSpTBXItem
      Action = CommandsDataModule.actEditDelete
    end
    object mnEditSelectAll: TSpTBXItem
      Action = CommandsDataModule.actEditSelectAll
    end
    object TBXSeparatorItem9: TSpTBXSeparatorItem
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
      OnClick = mnUpdateViewClick
    end
    object mnCloseTab: TSpTBXItem
      Caption = 'Close Tab'
      Hint = 'Close active tab'
      ImageIndex = 52
      OnClick = mnCloseTabClick
    end
  end
  object SynCodeCompletion: TSynCompletionProposal
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 200
    EndOfTokenChr = '()[]{}. =:'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Resizeable = False
    Images = vilCodeImages
    OnClose = SynCodeCompletionClose
    OnExecute = SynCodeCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    OnAfterCodeCompletion = SynCodeCompletionAfterCodeCompletion
    Left = 452
    Top = 33
  end
  object SynParamCompletion: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. ='
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Images = vilCodeImages
    OnExecute = SynParamCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    Left = 456
    Top = 87
  end
  object SynWebCompletion: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 340
    EndOfTokenChr = ';>()[] .'
    TriggerChars = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        ColumnWidth = 100
      end>
    OnExecute = SynWebCompletionExecute
    ShortCut = 0
    OnAfterCodeCompletion = SynWebCompletionAfterCodeCompletion
    Left = 456
    Top = 136
  end
  object vilGutterGlyphs: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'EditorGutter\Item1'
        Disabled = False
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'EditorGutter\Item2'
        Disabled = False
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'EditorGutter\Item3'
        Disabled = False
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'EditorGutter\Item4'
        Disabled = False
        Name = 'Item4'
      end
      item
        CollectionIndex = 4
        CollectionName = 'EditorGutter\Item5'
        Disabled = False
        Name = 'Item5'
      end
      item
        CollectionIndex = 5
        CollectionName = 'EditorGutter\Item6'
        Disabled = False
        Name = 'Item6'
      end>
    ImageCollection = icGutterGlyphs
    Left = 168
    Top = 80
  end
  object icGutterGlyphs: TImageCollection
    Images = <
      item
        Name = 'EditorGutter\Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000000974944415478DA63FCFFFF3F03B180713828BE79F36EDDD1A39F8225
              2519DEB9B8E8A4B0B2B2DEC5AAF8E5CB5751C5C51F17FEFAF59DE5FBF7170C99
              9942E7BDBC4C8CB02A3E7AF4F2CA9E9E0F61B76FDF62B876ED19435F9FE5E3FC
              7C670D4646C66F584D6E6838357BC68CB35C4C4C4C0C5BB7BA9FF4F030B3C0E9
              E6DBB7EF55DDBCF9CA8D8F8FFDB395956E3E0B0BCBBDC11ECEF800008E4363E5
              991F7E5B0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001508060000004F3E6E
              D2000001074944415478DA63FCFFFF3F03258071D480616B00508CE3DAB59B13
              DEBC61F1F8FE9D894D44E4FB4D43438D586666E627441970EEDCB5DD9D9DAC4E
              FFFF73313130BC63E0E2FAC2909D2DF6D8C444499B9191F1335E037EFDFAA531
              6BD6E393870FFFE5FBF3E703C38F1FEF199E3C79C6606626FAB7AA4AA75A5151
              A113AF01EFDFBFF7EFEC7CB4E6FAF5772CEFDEBD06BAE61EC3B76F3F19949585
              1856AF765A6B68A81D42D0051B37DE39DEDB7B56E0DCB9870CBF7FFF058BC7C4
              E8FC696E36A9565090EF22220CAEEE9939F38ECDAC5917D8417C1D1DD1FF4B97
              BA3DD0D555D6271806D0586007C5C2F7EF8C6EFFFEFD676265FD7B5B4F4F2301
              180BCFE8930E460D200D000086BAAFD7DD0694960000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000160000001C0806000000652F4F
              5D000001994944415478DAEDD4BD4FC2401400F0775AA18545A526367C0C828C
              18FF0937665D6413E3405C584CE00F602004E342427490D17FC1C55D02216902
              2289546AFD2A8522B4F4CBE2401CD458898BF2964BDEDDFBE57277EF906118F0
              1B81A6F0149EC213C29224AD379BCCB62CAB7EBB1D6B78BD9E0241109713C1B5
              DA5546511676B2D936D1EF1B3314A51A910839B0D984936070358610322CC3AD
              16BB572CA2743E8F1176BB6C6644D075D11CFB108BADF4FDFE99038FC7736809
              36F388611E1FE2F101899062825D50941EC8720F04410082D0A150D86853D492
              EBA35D7F0A6B9A46954AED7A2AC539545584E150049EE781A66FA1D379795B73
              7DBDDBF5F9C81086613796609A6ED713898A439244E0B867A8541878BFFE47F0
              281A8DFBA764B2E4A2E91694CB2D13D5C77381800B2E2E36F9E56592B47414A3
              60D9BB28CB2A9970F8CCC171E238EF74DAE0FC7CABE776DBCCCB731F597E15A3
              A8D5EA691C9F8FE67265A25AE56783C1452D1A0D0D8643E1D87C6EFB9FD57DB7
              41D618E6765B9294551C9FAB9B0D728AE378E9AB9A3FFA574CE17F06BF024D4B
              0BD8D5F26D220000000049454E44AE426082}
          end>
      end
      item
        Name = 'EditorGutter\Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000001404944415478DA63FCFFFF3F03B1809122C5403E0B10F33232327E01
              E2DF78153F7BF62CE3FE6FAE569E5F1F9FE92ACBDA333131BDC3A9F8C9D3A739
              0507A427DF7FF7FFFF3C8F67777594241C9899999F82155FB87C73E36F065E7D
              A01EB02E16662696EC99E232C7AF3030A8CB3230AC287B764F474DCC998585E5
              01E3F90BF72F18BB2BE8FF83B9EE3F2A96956160D8B8ECE5132D0D014FC6F3E7
              EF5D3036E6D4FFF7EF3F926A645D0C0CE2E24C0C478FB2BE062ABE0D54FC04AA
              F83F56C5B367CBBFB3B2625809547CE3FCE2C5CFD4FEFD832801695ABEFC01D7
              EBD75F19181919806CBBB7A6A6FCBD4A4A0AED8CDFBE7D330662255868BC79F3
              C6A3BAFA52D2C68D3719D6ADF37D6B602052272B2B3D0D7BD03D799A3369D2F5
              C9AEAE126FF4F4C40AC4C5C596E20CE79F3F7FEADCB9733F4756566A351F1FDF
              5EB2D30600C7EDA6A71FD52DE40000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001508060000004F3E6E
              D2000001E84944415478DA63FCFFFF3F03258071181BF0E1C307AFFFCCACABFE
              01157C7AF7A65B5141A18924035EBC789174F717EFA48273DCDCEBED7FFCF8F9
              EED9046565A54A920CB8F29E67A2EB021E1E592106868309DFBFFFFEFC74969A
              8A4A018A01CF9F3F4FFCF4E98B3EBA01FF18FE6B3FF92A65E9DACECDCD00B443
              468481E1501DD0902F4FE6AAAAA8E4313232826D66BC79FBF18D63B764D4AF3D
              40D20D75D4F9EB0C0C7B4E22F8922043667DFFFEE7FB9325EA6A2AE92043186F
              DE7C7CA3B059467DDB2E24CDFF510D42A6454519180E6CFCFE9DF1FFE3351A1A
              AA0910030AC5D5B76DFB87A49201AF2922228C0CFBF7337C67667EB20D68C043
              A001EC4003BEA229C46F88810107C3B97372C030B8F90068C057A001EFB028C4
              AE59569683E1D021ADEFBF7EBD9E0534E03ED0802740039EE2D0802A2627C70D
              D46C07D4FC7E9AAAAA7209E3AD5BF7AFDFBFFF4FF9D9B3CFFFD035ECDAF58475
              F9F21B4C3031353521A0DF837FFCF8F1AE4B4949A91E1C8DEFDEBDF3FDFCF9B3
              167A3AF8FEFDBBC9E3C70C1EAEAE2B78407C4D4D51867DFBA2809A3F362B28C8
              B7119712AF7C9C0832404303A439F2C7EFDF5F6AE5E4647B884ECA0F1FFE98DC
              D0708C6BF162DF9F409BCB64646426A1ABC36900D05B8EEFDF7F5E0294666463
              63AA93949498834DDDC0172800AF162134A08060D90000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000160000001C0806000000652F4F
              5D0000029C4944415478DAEDD55F4853511800F0EF6ED7ED6E4D37BB0B976D75
              C3E97CEB3ED44B1462113D24B41456D9430A460F890D24289FEA315F56460F4A
              0A8AC4E89F8895841A464F8126F412FBD3FEE8A2D2C2748DB579774E67E6B63B
              E7627305119D97F3DD73F97EF79CCB77CEA130C6F0271AF51FFE3B6084101B89
              44B8782C93C9DE4BA5D28FBF0576B9DD2F2B8DC603F178F95B2824017C4CA552
              BD2818F6787D331E39C75F9E02B0ED8BC5F66E158448385CAFD1689E160C4F7F
              E778CB23008606186A40E83087F1D2D7C546AD567B6FF3B087C04B04EEF9F94C
              13FC6E33C6F57B305E58983FAFD3E9EE64C0C160F0E0A7F9CF5D18283A1B5CBE
              BDCCF86486612C9D6B03642E9404A0AF15E3B38700028140BBC160B0A5C17373
              7356BD5E6FEB7D0C1043A944718B2FCA3E063039257A17EF29809BED006D2701
              7C3EDF358EE3AE66C0F26A8068645D22AC8BB38C5D6A05E8BC02E076BB6F5754
              54B451148552B086C0E1FCD1447F81E0B7BA009C4EE760555565730A9607211A
              DD283BF7B8A58581EE6E2576B95C0F44B0770D2E0C1F19D1435D9D4AF48FE553
              048E1584DA6CBBC16ADD41CAD3735D048F12186D0AA54875F4F6F2B8A96967B2
              F444F0208185BC6749D312B0DB6BB1D9BC2BBE59CE91CDD2B7FA31025F24F08D
              C9499F8050621B66EC466A60C041F7F7BF4943158A22181E3E816A6A0C28185C
              3EC5B2ECC36402390EABFD7EBF1556CB7DE3C6B2DA8689890FACC5723F395652
              C2C0E8E8E918CF6F135656A2C7D56AF5B3B499E476567867A6A7C37C02D66AB7
              C0D85863CC685447314675C5C5C5CF3396982F5C56A682F1F133825EAF08C964
              4547954AE5AB8D727286171719BEA7E7357474EC171846F8525AAAAE25B7C9DB
              6C3939C10E8763C8643299E3F1EC6CE05D79B9EE084DD3DE5FE5FCA3B7F466DA
              0FD65B7D3049D7B8380000000049454E44AE426082}
          end>
      end
      item
        Name = 'EditorGutter\Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000001504944415478DA63FCFFFF3F03B180913E8AEFDFB891F9FDCD87E6CF
              E24A7C6C772EBF125653A89353569E87A1F8E8AC595B8D7EFDF27AE71CC060B8
              5E86214CFD174395C47586BBF72E2DB78D8D8D022BBE71E7D9FD672FFEF3F1B2
              730B310035FE61646570AEE366F8F68D8121C0FC37C3149F5B0CDFF8FF16ABEA
              E9F5319E38F5E2B185B3B80C03C80264CC00A1F5B4FF312CE9BDFB55D35A518B
              F1E4C9A74FCDCD7F4961A84262DBD87030CC9CF9F732E38913F71F5B585C91C1
              A5584F878761F144D1EF5A76DA1A8C376EDCBFFFECDE7B41DEDFFFF8197FFF66
              F8C3C6CEE014B107E8E69F0C019E0A0CD3D224195E70FFEB3174752D05870610
              F3EC2B29D9E5DCD767F97CC94606FDC25B0C113E0A0CD5567F18AEDC3C7BC4A9
              ABCB939191F10B3CE880B4C0998D1B9B383FFF8EFFCA2BCBC3F6E4CAC7BFB2A2
              8B8DFDFCEA800A3FE28C41A0181750C1378AA21B00DCC6AC67B5253243000000
              0049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001508060000004F3E6E
              D20000026F4944415478DA63FCFFFF3F03258071181B0014674451C8C8F89F28
              03807CA6BB376E64CA29AB4CFA0BD4F6E6CAD5DFF7D6ADBEFF5F4BEB94657070
              3B3B3BFB759C06FCFDFB577C575BDB16077E7E63868C6C46EE99CC0CAB5D7F33
              B8B13C63609B3793E1C6B3679FFEA7A6F619585B37621800A4B97655559DB597
              97D760646161F81793C8C055C20C740303C392E8DF0CBE22CF1958BB5A18DE3F
              7FFEE7656565B381955513D88027CF5E3DE3E2E21160666264656306EA048B32
              307CFCCAC02011CD0277DD9CDCDF0C612A4F1938CAF218CEFDFEFDD670C3061B
              3676F61B8CFF8080591B18427F40CE800504128DC46E2FFCCD90EDF08881D7D5
              8AE1D0D4A933EC4242322106B0010DF88BA418355851E8AAD2DF0C5549AF180E
              2F9C76DAA3BDDD0C68C0FFFFCCCC4F192041F11FAB26747A420F1F4388DBDB5F
              D2BAAAEC1017309F0206E63FA234EBEB73331CD8A6C27079E1B47BB69515CA50
              03D64063035D13AA01C6C6C20CFB369B33F0662430EC76735DED969D1DC6088C
              FB7F3B76DCFCF0FFE73F1E7686FFAC60BFFCFDCB70F3C157869CB2A370CDF6F6
              720C5B56B833F04406333C3B7DFACFDFCB979364151517337EFCF8D1F9F7EFDF
              42EFDFBD53FEB9787199D6C38782A034FC63D234064E811EB0764F0F1586D5F3
              3D18B8037D18FE9F38C1B0A9B373AB7F59990F464A7CFAE851E49DD8D82EFB8B
              17657E3C7C0A36C0D7478D61F92C57066E3F4F865F67CE306CAEA83814D8DA1A
              C8C4C4F40E6B5EF8F6F5ABC98199339BDC72F23C372E3FC7E0E12CCFF025C0FB
              DF4925A55B6249495BCCDDDD6B8019EB27C1DCF8F2E5CB98174F9F1A313130BC
              9353523ACCC7CF7F04A8F12FC1DC482AA0D80000ECE137E613E2848D00000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000160000001C0806000000652F4F
              5D0000037D4944415478DAED957F48535114C7BFEFBD6DDA667B8A96B1AC597F
              88300786AC5F6AB0863134A40C25A3092119152EFAC38944691A0656A0453FC0
              D94F887E2CB392A44DA3DF44BF6D432651361D89594E2D87867BEBBE599A6396
              09517F7478F0EE3DF79CCF3DDC7BCEB994D7EBC59F10EA3FF8DF01133BCAE740
              519372F82998AC899C1D1DEB299AA9A4052221E5F5785B9F3C7EBA60D9B29D52
              A9F41ED964F8B7C044C7D8ADD66DAF4E9FDEB22426267A465E1E53FA12081501
              FA58C075C3CC3559CCAF143939C76295CA836403EE976032975CAAAAAA533B9D
              CB25F3E7D3B45C0E515A1A94C7015B17A053022752BDE06C36BCDB5731FC50A5
              B264E5E76712F8C0846032165EA8A8684AFFF225998A88E0E76064328856AD82
              722F01778CD8A949D45737721075B663A8A810D712121AB30B0AB404EE1905DB
              EDAD26C97476313F6140B1614141216004FC36BE8F1130104E9F06453ED0D23E
              A2E6254E0E584A3C081FFA808FFAAD9E379B371F58AAD1148E827B5CBDBDD6B7
              2C5BFF60CC69FCD9007D9F01E365801B1ED3F1229B49E0873C888970C35C56DC
              A9DDBF3F8EA6E99E11704F6FAFF13CCB1AF6FC00F6FFFF44279600B5D51EA424
              737870EFCEB9248D267B0C5C43C00501136E52639A068E1CA690B78986C3E1D8
              1D1D1D5D3202360A598361E0B76081C63B7684A0AC4C8AB6B637E594CBF5E953
              75F56088C1D03505A83F3892806711705BF9B788FB48C4AFA71C2D4D53387A24
              161BF3A2FC8FA293809BA704168B05A8BD9888144D249AAED4DD4AC9CA528F5E
              9ED5FA99ADAF772070BE9174EB1B82D16803C771E36C64B210581AD2112313C2
              9EBB6130EAE4C9B4D0B0B09B3EB0DD6E374924525F8108382E228CA2827CBE1C
              29228EDCB83818C25933A1501C454BCBFB51685C5C2481AE45F8600F84DA1538
              A3D3DDD415176BBEAF8F2B69B7DB9D705DA7ABCDB87F7F2ED5DD0D8A8F50AB05
              1A1AA0541E838D6F1644D4EA79B87A3913C1761B042B53715EA572A49B4CABA7
              89C5CF03827971B6B7AF7B949B5B91D1D838DBA7F003E7E4C4E3B87125507F0D
              CCBA6CD426263A17D6D41446C9E5677FE4046C9BFDFDFD49178B8A2A579C3A15
              3F273999E1C1A5A5B7111A1A0CBD7E11BC959570EEDAE531EB742F32CBCBB74B
              59F6AE3F63C2464FF48296E6667DABC5B226292353C5906EC41BDBEA4CAE4EB7
              FB992235D5AC888FAF9AA8D94FFA69221911CEDB9326F36132F67FFF31FD67C0
              5F0138B3BC16D36FE0830000000049454E44AE426082}
          end>
      end
      item
        Name = 'EditorGutter\Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              950000012D4944415478DA63FCFFFF3F03B1809122C5FFFEFD137BF9E489330B
              3BFB571131B11D8C8C8CBFB02ABEB073E72499E3C7D3B9F9F9D9FE7FFECC70E7
              E3C767122525B9629292EB5014DF3C77AE42F6D0A176A65FBF18FE7FF8C0C008
              C2EFDE313CF9F3E783F49225761C1C1C97E18AAF96973F57E2E292607CFF9E81
              0188996FDE6460BE758B81E9D32786338B16CD32898A4A072B0662F64799991F
              C5FEFD63677AFB9681E5D83106E6172FE0CE3B5653B3DBAAB9D90D6EF2C5B4B4
              E71A8F1E49B05CB8C0C0FCF2258AA74FCE9CB9D43C2D2D06AEF8DCF6EDFDBA39
              3905ACF7EEA1287C2924F483F5E6CD402111911D70C5409AEDD8ECD9F30D7273
              C3B97FFD620689DD5750F8F86BD5AA367553D32EACE1FCF6D52B9FE7D7AE9931
              B3B1FD543034DCCAC9C979813A314835C500A419A4E5A8048AC7000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001508060000004F3E6E
              D2000001F84944415478DA63FCFFFF3F03258071F01A001467FCF3E78FC2AF9F
              3F4539B9B8EE303131BD23CA00209FEDC4E6CD13B88F1D0B9165631361FEFB97
              F1F5E7CF3F1E88885CD64B4F6F111517DF84D300209BE74063E3314B09095D86
              7FFF18FE7FFFCEC000C48C5FBE30FCFFF68DE1D5FBF7DF3FE7E6B6E89899B561
              3560EF94293BAC9998DCFFFFFE0DD6C800D4C808D4F8FFEB57B02120FCE4D7AF
              AF028B16850A8B8A6E4731E0FBF7EFFA9F1A1ACEF08988B0C035033532403533
              3D7DCAC0F4EC1903E3E7CF0CBB8B8B77B857577BA218706EDFBE0ECD63C7CA19
              8036C2348230D3A3470CAC972E810D8181C34646AF6DCE9C51666464FC0C37E0
              C8B2658B8DCF9E8D016906E1FF400DAC57AE3030DFB98311F28FD9D9FF8B7EF8
              A0CBC1C171156EC0892D5B261BAC5D9B03B61DE84C969B371998EFDFC71AC517
              6564BEE93C78A0CCCCCCFC026EC08BA74F433892925671FDFAC5C8F4EA1503CB
              B56B3813CFF6B4B4339E33679AA284019066DA595E7EC265EB5653E67BF71818
              7FFCC0AA19247AEBF8F11A3D0B8B568C68FCFCF9B3D5A5C0C055D67BF74A63D3
              FC07887774776FF62E2E0E0406E05FAC29F1CBE7CF96079A9A3AAD264DB216FA
              F58B09267E464BEBFDB7868655B62121F940CD3FF1E6059077DEBC7AE5FDE8CA
              15CBBF3F7E70708A88BCD130325AC1CACA7A8F605E20158C1AC0C00000BFCD2C
              E6339F65FA0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000160000001C0806000000652F4F
              5D000002E04944415478DA63FCFFFF3F032D00E3A8C164190C54CB08D6C4C848
              5013418381F2EC2F5FBCF0BE71FCB8FBCF77EF44983939BF0B292ADED637379F
              C3C4C4F40C9725380D06B9EEE6E5CB05CF162F2E306167976593906064F8F387
              E1FFAF5F0CFFDFBC61B8FCEAD5A7571616FBBC5253339999995F106530508C79
              C7C285CB2DEFDC09E694956562F8FB97E11FD040C69F3F19FE0331C3F7EF0C8C
              DFBE31307CFCC8B09D8BEB965B77770C3737F76982061FDABC79AAE1F9F399AC
              A2A28CFF81AE4431F0C70F06061806F13F7F66D82C25753368DA343B60D0BCC2
              69F0F7EFDF0D1F94971F515051E16204BA146C2010835D88642823D05086AF5F
              21F4A74F0CBBB3B2967B27254561351814AEDB66CD5AE7F2E143C07F4646ACAE
              045900328CF1C50B062610FEF0818101184C5BCCCD9F79EFDBA70774F55B6C06
              33ED2E2ABA672B2C2C0F76251617323F79C2C07CEB160323D095603D204380F8
              0BD0218FCE9F2FD2D2D7EFC76630EF89F4F4C7860202FC2057800D8486250330
              2C592F5E64607AFE1CA40E6C183A38B861C3147B7FFF5C0C83FFFEFD2B7E2E39
              F9B60E17172FC8C54C2003411818FB6CE7CE81696C06C2C0DE152B663B8787A7
              617331F3CED4D4C70E3F7E4882520203283C81DE67BD748981F1FD7BFC190988
              4FECDEDD65E9E2528E35F236B5B5EDF73C71C21E1CB64083991F3C60607EF68C
              8110382F29F943E5E64D675E5EDE635893DBB50B178A84B3B2BA857EFF660225
              23963B771818FFFD2368F0F2828213117D7D56B02C8E613028389657541C0D5F
              BDDA9C11987599809146089C5551F924B96F5F9A94ACEC4AACE918067EFFFAA5
              B22E3E7E6BD88A156A8C040CBD2D26F6E3D5C28553AD3D3C4A90C5711642BF7F
              FF565E555BBBC07EEA540B992F5F58D0E54181B3C5D9F9B1427BFB343D53D30E
              7479BCC52628C3DCB97123FDDAE6CDDE3FCE9C51E178F182EF370FCFCF7FCACA
              AF44ECEDCFDB0504B4B2B0B03CC6A697D4829E1B487D074610C1D81C1C55D3A8
              C183CB60006E02CBD8242C257E0000000049454E44AE426082}
          end>
      end
      item
        Name = 'EditorGutter\Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              950000014C4944415478DA63FCFFFF3F03B1809122C5BF7EFD527FF3FCB909BF
              B0F01D6E1E9E9358150369BE93D3A7AFD3FEF8D191959D9DE9F7AB57FFAFF2F2
              9E352A2D8D616363BB89A2F8C48C19BB0CFEFC716560BBCBC07087958181ED21
              03E3CD3F0C67A465AF58F5F79B303232FE042BFEF9F3A7E6F7BABA2B1CD21F99
              38726632FCD9A6C1C0E27093E17F091BC3FF59BF181E5FBF9E26AFAE3E1BACF8
              D9C387514213262C65F8F08181D5FB000373C8030686C340EBEC206EBDBC7B77
              9BAE8B4B3558F1E74F9F6CFE27261EE2E07ECAC83605E8A78B401556401C0AC4
              EB81AE3A7FBE50C5C06002583110331E696E3E603375B21DA3DB1B0686254045
              0140BC8F81E18CA5FB1DC3AD5B6D9899995FC23D080C32B5936565736D264EB4
              618406D5192FAFDB4A0B17E6098988ECC00867209BE5F19D3B719F9F3C516417
              127AA3A4ABBB9C8989E9157562906A8A015B12A5E5E19A34D30000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001508060000004F3E6E
              D20000023A4944415478DA63FCFFFF3F03258071F01A0014E7F9F9E387120B2B
              EB076666E6278C8C8CFF8832E0E3870FF617A74DEB95FFF2454B909999F3C7A7
              4FFFEE3131BDF8E7E8B8C1D2D7B71468D0379C06DCB97A35876FD5AA5E3E6969
              B6FF3F7E30307CFFCEC0F8E50BC3FF6F403D9F3E311C10163EE7DED6E6CEC4C4
              F406C380AF5FBF9A7E686D3D242C2DCDF1FFD767A0C43B0686978C20090646B6
              570C0C8FFF33307EFECCB0CFDD7DAB4761A10F860107264EDC63F1FFBFF3FF9F
              9F19D8FC17313009BF66F8D5E5CDC0A47A878135E914C3DF4A6906C645BF199E
              B1B0FCE2BA74C95748587817DC002066BC5054F45A9397571864007BCC3A0626
              DD470CFFDFB231300AFE6260F80B54190AC41B21B69EDCBC7982B98F4F21DC80
              7FFFFE09DEC9CC7C2ACBCECEC90872F28F4F0C6C0DDB191855BF4274A402F11C
              44C01D9C3D7BA97D4A4A0CB20B58CF2625BDD2F9F54B001468CCD6D719584A6E
              333030417500990CF640FC1CC23DBA7CF92CEB8888749430D8535777C2F6E449
              73A6BF1F1858979C626010020A4601711C10FB01713B10573130003DC4F0F0CA
              951C556DEDA92806DCBD712343302A6AAAC0AB574C4CBC4F1918948082DB8098
              058841764D03A52EA0507CFC45CFF9F3CD81E9E127463AD83D6FDE52EBCCCC48
              AE5FBF18B1A5BA53C6C6AF94B76F4F101615DD8E352101D94C6776EF6EFF555B
              1B6779EA94042C083EB2B0FC3F929575C2BAA1A1464050701FC1BCF0F7EF5FF1
              5B972E457F79F1428A8593F3BBB486C6195171F1CDD8F2C320CECE43C7000034
              DD18E6F1A5BE460000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000160000001C0806000000652F4F
              5D000003324944415478DAED956B48536118C79FF79C6DCA0C4C561F1C79372F
              441618515EC20F560BC21596B6840976115150F463CCF5A9BEE49714C2483323
              EF223AB20B658EDA3EA82C10A38419E6A5A5CCBC3437773D3DEF36516B33FD10
              F5C117FE70CE79DEE7F73EEFFF3CE73D84E338F81B83EC80B70DC679C49D40C8
              961236055398CBE50A1D1E18B8323F3111EDB058840122D16C526A6AF7EE9090
              37B88873DB6087C311FEBCB6F65EAC5E9F1121160B49602070361B90E565304E
              4E3A878283470F1715DD898C8979B865F0D2D252BA4EA1684D898B0B75B12C70
              0E0710AB153814582C40506036C394D1B8329597579F219516FF118C9546BCAD
              A8D01E8F8F17D308C12ADD40144118ACAC0087625014FE6D71D16E54286E27A7
              A529FD82A9A7AAEAEADED30E8784E3F1DC40364207607382F37D981B4AB81FC0
              930C81F3710C6ECD638D5A249AC9E8E848E1F3F99F7D82B1DAC8D1B2B291E8C8
              C820B0DB113A0C829C569C85ECFB99E0D2ED0141D94B600ECC01A71502270B02
              32677617F0BAB1B1EEA44C76D52758A7D5DE4C50A9948C1093709BECFE0F2090
              AB00787455DCD12482A2963D939FA1CEA3AC9EDBB6E2E2C19C9A9AA33EC1AF9A
              9A1EA50E0FCB81BE2CEA21F532610C04A5FD00FC7506BE4065A16C6B8F3A2492
              2FD9BDBD31AB2DB801FCB2A1A1F9C4D0D0256A83DB4FFAF65D8B20B8D50F246E
              1DE502AA73631774A7A71BB2D4EA58049B7F03F77777D71C6B6F2F264EA7A7AD
              3813F0155A20472C1B29762FBC67ED515B6EEEA79C9696449F56CC198D6796F2
              F27AC40CC3A3607EC14760E4B39EE053541DAA1915805A4045D0A6F7565C55D5
              232D2F97FA6B375E6769A94E3A387890F6283B3309CC8379001683E7BC9E9EF2
              C2B3E9163D79632121366660A0282A36B6DEEF0732AED7179864B2EA448341C8
              4C4F0311D015BDDB5F1DBB5026CFA58BDAA054F6E52A9599EB0F289F9FB45AA5
              BA1B5658783DDA6008804D06853EC9CF1FB95C57276159767A7DCCEF21A4D368
              2AA72A2BAF9DEDEBDBC7F8888F8944D6C192927717150A3942BFFE1ADFF4D8A4
              279CBAABEBC6778D26891B1FDFCB3399029C62F1426072B2FE50565657785454
              A3BFDC6DFD4170AE007DB46D65EEBFFF35ED80FF3FF04F5595B1D8E339E61900
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'EditorGutter\Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000001114944415478DA63FCFFFF3F03B180916CC54036F3FBF7EF5DBE7FFF
              2E2A2E2E7E888585E51156C54045EE2F5EBC58A8A8A828FEF7EF5F86376FDEFC
              04F21798999965333232FE852B064A4A3D79F2E486989818EF9F3F7F407C3006
              B15FBE7CD9ACA7A75707577CEAD4A9395A5A5AC9304520FCEFDF3F30BE7EFDFA
              5347474725A0E9BFC08A8F1E3DBA17A8D809A60844C3C0D7AF5F19646565B5D8
              D9D9AF83159F38716291B2B2722CCC346470FFFEFD0F9696967240933F831503
              759BDDB9736737D0CD7CE8C105543CDBCACA2A0D25341E3E7C98FAECD9B31605
              050531101FE8B9FF376FDEDCEAE4E414C7C4C4F41E239C7FFDFAA57EEFDEBD00
              20CD2B2222724552527225D0FAFF18E14CDBE82604004957BDE50C68102C0000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001508060000004F3E6E
              D20000019C4944415478DA63FCFFFF3F03258071F01AF0F7EF5F89EFDFBF2B70
              72723E6266667E46B401B76EDDCA7BFBF66DA1A2A2A22C5033F3870F1FFEDCBD
              7BF7819898D81C6D6DED2E4646C6FF380DD8BB77EF4A4B4BCB5016161646A00B
              40AE60F8F7EF1F18035DF3FFE2C58BEBDCDDDDC38086FCC330E0F2E5CBD54A4A
              4ACD40E732C2348268903CCC901F3F7EFC7FFDFA75AB9191512D8A0140CC74E2
              C4892706060692D83423D340750FFDFDFDB580AEF80637E0E3C78FF63F7FFEDC
              0FF433DCE9D080846B8401A03A062626A6383939B9C57003EEDCB99321222232
              1DC486D90CA3B181376FDE54EAEAEA76C00D78FEFC7928D0D45540CC00F512D8
              666C0024F7F9F3E73C7575F5C9C861C0BB7FFFFEFBC0681286D98ACBF66BD7AE
              7DB2B7B7D703C6D44394580046E142A00171F83483C0850B17567B7A7A866144
              23901658BB76ED6E6B6B6B135C9A8F1D3B76392020C01318D54FB12624A0BF45
              77ECD8315D5050D04B4141811326FEECD9B35F40BC0B68730ECCE978F3C2B76F
              DF8C6FDEBCE9034C7D42ECECEC1F55555577F1F1F11D212A2F900A460D606000
              008D493CE6C2B901170000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000160000001C0806000000652F4F
              5D000002544944415478DA63FCFFFF3F032D00E3A8C1241B0C54C7080444BB02
              A7C120838058F0D6AD5B314F9E3C31FCF5EB97003B3BFB3B7979F953CACACA4B
              81967C21D960A018E7FEFDFB27F1F2F2066B6B6B0B32333383C418FEFDFBC7F0
              E5CB1786D3A74F3F131010586165655501B4E037510603358B6ED8B0618F9797
              971E131313DC4074FCF5EBD7FF8780203232D21768F867BC0603D92C6BD7AE3D
              E6E7E7678A6E20321FC406E1DFBF7F3300CDDE111616E6851EFE28065FBC78B1
              5A4949A9998D8D8D1164C0DFBF7FB11A0863030D6378FAF4E91F1E1E9E1C1515
              9599580D06D24CDBB66DBBE1E4E4A48ACDA53043910D87EA6300C6C7A1A8A828
              7BAC06FFF8F143EFDEBD7B67E4E4E458416220D7C234A307053A3876ECD8DB80
              80005D60243FC730F8FEFDFB49DCDCDC7381490AC385C8AEC7069E3D7BC6A0A8
              A8E82E2424B40BC3E06BD7AE158A8888F4B1B2B2E2341814A6D8C0DBB76F1980
              C92F524646660586C16FDEBCF17CFFFEFD56414141466C61890F5CBF7EFDB7A5
              A5A5192727E7056C91C7BD69D3A65B40055248620CC464F95DBB765D8F8B8BD3
              03FAE80FD6E406CC186B8106079162E89F3F7F182E5FBEBC0098A11271A66360
              8257D9BC79F301605695C617A6C860F7EEDD77A2A3A36D8129E2054E8341E0C1
              83070957AF5EED313232122664E8F1E3C79F5B5B5B678B8B8BAF4797C35A08BD
              78F12268C78E1D2DF6F6F69A1C1C1C18F2C07282E1C89123978059BF14398911
              34181AC63C4017D53C7CF8D01258644A03D3373790FE02A41F01B3FD11131393
              766050FDC0E51B520A7A5E6CA518C506930A460D1EC20603002A5EF4D8FDDFED
              660000000049454E44AE426082}
          end>
      end>
    Left = 96
    Top = 80
  end
  object vilCodeImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'CodeImages\Item1'
        Disabled = False
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'CodeImages\Item2'
        Disabled = False
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'CodeImages\Item3'
        Disabled = False
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'CodeImages\Item4'
        Disabled = False
        Name = 'Item4'
      end
      item
        CollectionIndex = 4
        CollectionName = 'CodeImages\Item5'
        Disabled = False
        Name = 'Item5'
      end
      item
        CollectionIndex = 5
        CollectionName = 'CodeImages\Item6'
        Disabled = False
        Name = 'Item6'
      end
      item
        CollectionIndex = 6
        CollectionName = 'CodeImages\Item7'
        Disabled = False
        Name = 'Item7'
      end
      item
        CollectionIndex = 7
        CollectionName = 'CodeImages\Item8'
        Disabled = False
        Name = 'Item8'
      end
      item
        CollectionIndex = 8
        CollectionName = 'CodeImages\Item9'
        Disabled = False
        Name = 'Item9'
      end
      item
        CollectionIndex = 9
        CollectionName = 'CodeImages\Item10'
        Disabled = False
        Name = 'Item10'
      end>
    ImageCollection = CommandsDataModule.icCodeImages
    Left = 100
    Top = 127
  end
end
