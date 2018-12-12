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
      Images = CommandsDataModule.Images
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
        ExplicitWidth = 576
        ExplicitHeight = 390
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
          CodeFolding.GutterShapeSize = 11
          CodeFolding.CollapsedLineColor = clGrayText
          CodeFolding.FolderBarLinesColor = clGrayText
          CodeFolding.IndentGuidesColor = clGray
          CodeFolding.IndentGuides = True
          CodeFolding.ShowCollapsedLine = False
          CodeFolding.ShowHintMark = True
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
          FontSmoothing = fsmNone
          ExplicitWidth = 365
          ExplicitHeight = 386
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
          CodeFolding.GutterShapeSize = 11
          CodeFolding.CollapsedLineColor = clGrayText
          CodeFolding.FolderBarLinesColor = clGrayText
          CodeFolding.IndentGuidesColor = clGray
          CodeFolding.IndentGuides = True
          CodeFolding.ShowCollapsedLine = False
          CodeFolding.ShowHintMark = True
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
          FontSmoothing = fsmNone
          ExplicitLeft = 372
          ExplicitHeight = 386
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
          ExplicitLeft = 367
          ExplicitHeight = 386
        end
      end
    end
  end
  object imglGutterGlyphs: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Height = 14
    Width = 11
    Left = 96
    Top = 80
    Bitmap = {
      494C01010600080004000B000E00FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      00000000000036000000280000002C0000001C00000001002000000000004013
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C0C0C0D010101020000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F0F0F110303030400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000141414174242
      5D95292996D81616BEF02222A4E23E3E6CAB2A2A2B3600000000000000000000
      000000000000161616195C5C5C988F8F8FDAB4B4B4F39B9B9BE56A6A6AAE2D2D
      2D39000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001919191D31318DD00000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF1B1BB9E83636394E00000000000000001B1B1B1F868686D1E6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFE6E6E6FFACACACE93A3A3A5000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000454573B32A2AFBFF021C
      FFFF0059FFFF0000FFFF002FFFFF0042FFFF0000FFFF28289ADA0E0E0E0F0000
      00006F6F6FB5E7E7E7FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FF909090DA0E0E0E0F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001C1C1C204949D7F74444F9FF0BB2FEFF00FEFFFF0088FFFF00EDFFFF00E4
      FFFF0006FFFF0000FEFF41414A701C1C1C20CACACAF7E8E8E8FFE6E6E6FFE6E6
      E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE5E5E5FF49494970000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003C3C405D6666F6FF5C5CF7FF4857
      F8FF04D3FEFF00FFFFFF00F2FFFF002FFFFF0000FFFF0000FFFF42425D924040
      405DEDEDEDFFEBEBEBFFEAEAEAFFE7E7E7FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE6E6E6FF5A5A5A9200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039393C547777F5FF7171F6FF5183F9FF05F3FEFF00FFFFFF00FEFFFF006A
      FFFF0000FFFF0000FFFF42425A8C3B3B3B54F0F0F0FFEFEFEFFFEDEDEDFFEBEB
      EBFFE8E8E8FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FF5757578C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000C0C0C0D6A6AD2F48686F6FF30C9
      FBFF06F4FEFF4788F9FF0FD7FEFF04DCFFFF0E16FDFF0707F5FE3A3A3D570C0C
      0C0DCFCFCFF4F3F3F3FFF0F0F0FFEEEEEEFFECECECFFEBEBEBFFE7E7E7FFE6E6
      E6FFDFDFDFFE3E3E3E5900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004B4B68A19393F7FF8B8EF7FF6E93F8FF7474F7FF6270F7FF5169
      F8FF4B4BF8FF424282C50000000100000000676767A3F8F8F8FFF6F6F6FFF3F3
      F3FFF0F0F0FFEEEEEEFFEBEBEBFFE9E9E9FF7D7D7DC601010102000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000303030450507FBE9191
      F6FF9393F7FF8989F7FF7C7CF7FF6D6DF6FF494995D41D1D1D22000000000000
      000003030304808080C1F9F9F9FFF8F8F8FFF4F4F4FFF1F1F1FFEFEFEFFF9191
      91D61F1F1F240000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003E3E435F545489C25959A4D851518BC64545
      527F060606070000000000000000000000000000000000000000454545648A8A
      8AC5A5A5A5DB8C8C8CC953535382070707080000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0F0F110303
      0304000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001616
      161941415F9828289ADA1212C6F32020A9E53D3D6EAE2C2C2D39000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000403F
      3F54978989C73434344200000000000000000000000000000000000000000000
      00000000000029292A345F5B83BBAC98B3F056557BB329292933000000000000
      000000000000000000001B1B1B1F30308ED10000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF1A1ABBE937373B50000000000000000000000000000000000000
      0000151515173D3D3D4F15151517000000000000000000000000000000000000
      00000000000000000000000000005F5D5D83962B2BFFA17777E24C4B4B660000
      00000000000000000000000000000000000038383C511515CFEF786EECFF962B
      2BFFAD7695FF625CD1F53636394C000000000000000000000000454573B52A2A
      FBFF0202FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF28289BDA0E0E
      0E0F0000000000000000000000003C3B3B4D9F7777E09A3232FFA07777E03A3A
      3A4B00000000000000000000000006060607C6B9B9E6A98484DFA98484DFAC84
      84E3951010FF8C0000FFA86C6CEB615E5E85000000000000000006060607C6B9
      B9E6BD8E9DFABE8DA7FFBA87A3FF951010FF8C0000FFB1697DFF8075B2E70000
      0000000000001C1C1C204949D7F74444F8FF2323FBFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FEFF41414A70000000000000000000000000977C
      7CCAA43636FF820101FF800000FF877777BD0000000000000000000000000808
      0809BC8C8CE2C40000FFC30000FFC20000FFC20000FFC10000FFC00000FFC251
      51F37B7373A50000000008080809BF8D8EE6C40000FFC30000FFC20000FFC200
      00FFC10000FFC00000FFCA4D59FF83787AB2000000003C3C405D6666F6FF5C5C
      F7FF4D4DF8FF1D1DFCFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF4242
      5D920000000000000000000000009C7575CDD05454FFB14343FF901818FF8877
      77BE00000000000000000000000008080809C98C8CE2F90000FFF80000FFF700
      00FFF70000FFF60000FFF50000FFE05151F37E7373A50000000008080809CB8C
      8DE4F90000FFF80000FFF70000FFF70000FFF60000FFF50000FFEC4D58FF8376
      77AD0000000039393C547777F5FF7171F6FF6464F7FF5656F7FF2B2BFBFF0101
      FFFF0000FFFF0000FFFF0000FFFF42425A8C0000000000000000000000003C3B
      3B4DC76666EFD65656FFBA7C7CEB3C3B3B4D0000000000000000000000000606
      0607D1BFB9E6C69384DFC69384DFCC9484E3FF4310FFFF3400FFD9846CEB635F
      5E85000000000000000006060607D1BFB9E6E5A69FFAF1ADA7FFF0A8A2FFFF43
      10FFFF3400FFEF8A7DFF8B7EA8E000000000000000000C0C0C0D6A6AD3F48686
      F6FF7979F6FF6C6CF6FF5F5FF7FF4C4CF9FF2525FBFF0F0FFDFF0707F6FE3B3B
      3E5900000000000000000000000000000000151515183D3D3D4F151515170000
      000000000000000000000000000000000000000000000000000000000000625F
      5D83FF912BFFCB9E77E24E4D4C68000000000000000000000000000000000000
      00002B2B2B366E6ED2EFCAC0E8FFFF912BFFF0B894FF9089C3EE282829320000
      000000000000000000004C4C69A39393F7FF8D8DF7FF8282F6FF7474F6FF6767
      F7FF5A5AF7FF4B4BF8FF424283C6010101020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000041403F54A49D89C735353442000000000000
      000000000000000000000000000000000000000000000F0F0F11696879AFC7BD
      B1ED605E6FA20F0F0F1000000000000000000000000000000000030303045050
      82C19191F6FF9393F7FF8989F6FF7C7CF6FF6C6CF6FF4A4A98D61E1E1F240000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003F3F456454548CC55B5BA7DB5252
      8FC9464654820707070800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      280000002C0000001C0000000100010000000000E00000000000000000000000
      000000000000000000000000FFFFFF00FFFFFC0000000000F9FF3C0000000000
      C0780C0000000000803004000000000080100000000000000000000000000000
      0000000000000000000000000000000000000000000000008010000000000000
      8030040000000000E07C0C0000000000FFFFFC0000000000FFFFFC0000000000
      FFFFFFFFFFF00000FFFFFFFFFCF00000FFFFFFFFE0300000FFFE3F83C0100000
      F1FE1F01C0000000E0E00C0180000000E0E0040080000000E0E0040080000000
      E0E00C0180000000F1FE1F01C0000000FFFE3F83C0100000FFFFFFFFF0300000
      FFFFFFFFFFF00000FFFFFFFFFFF0000000000000000000000000000000000000
      000000000000}
  end
  object pmnuEditor: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
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
    Images = CommandsDataModule.Images
    Left = 96
    Top = 128
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
    Images = CommandsDataModule.CodeImages
    OnClose = SynCodeCompletionClose
    OnExecute = SynCodeCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    OnAfterCodeCompletion = SynCodeCompletionAfterCodeCompletion
    Left = 458
    Top = 32
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
    Images = CommandsDataModule.CodeImages
    OnExecute = SynParamCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    Left = 456
    Top = 79
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
end
