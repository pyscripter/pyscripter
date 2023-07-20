inherited FindResultsWindow: TFindResultsWindow
  Left = 362
  Top = 146
  HelpContext = 460
  Caption = 'Find in Files Results'
  ClientHeight = 358
  ClientWidth = 603
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    00002D2D2D4E0F0F0F8507070721000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00003A3A3A72E8D9D9FF0D0D0D810000001A0000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000001111
    116418181892262626BAD7CBCBFF0C0C0CBF0E0E0E8E0F0F0F850F0F0F850F0F
    0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F85111111642727
    277BEBEBEBFFD9D9D9FF9E9E9EFFC3BCBCFF828282FFADADADFF919191FF7070
    70FF7F7F7FFFA8A8A8FFDEDEDEFFE7E7E7FFE7E7E7FFEBEBEBFF2727277B3131
    3176EAEAEAFFE2E2E2FFD8D8D8FF9F9F9FFFB1AFAFFFA09D9AFFDEDAD5FFF4EF
    EAFFE8E4E0FFABAAA9FF939393FFDBDBDBFFE2E2E2FFEAEAEAFF313131763737
    3773EDEDEDFFE6E6E6FFE6E6E6FFBDBDBDFFAEAAA6FFE9DBCCFFF2EAE2FFF2EB
    E4FFF2EBE4FFF4EEE8FFB7B4B1FFB9B9B9FFE6E6E6FFEDEDEDFF373737733C3C
    3C71F0F0F0FFEBEBEBFFEBEBEBFFB1B1B1FFDBCFC3FFE8D8C6FFF3ECE5FFF3EC
    E5FFF3ECE5FFF3ECE5FFE5DCD1FFA4A4A4FFEBEBEBFFF0F0F0FF3C3C3C714343
    436EF3F3F3FFEFEFEFFFEFEFEFFFA2A2A2FFE9DACAFFE9D9C7FFEFE4D7FFF4ED
    E6FFF4EDE6FFF2E7DEFFECDED0FFA2A2A2FFEFEFEFFFF3F3F3FF4343436E4949
    496CF7F7F7FFF4F4F4FFF4F4F4FFB4B4B4FFE3D6C8FFEADAC9FFEADAC9FFEADA
    C9FFEADAC9FFEADAC9FFE6DACEFFB4B4B4FFF4F4F4FFF7F7F7FF4949496C4C4C
    4C6AFAFAFAFFF8F8F8FFF8F8F8FFD2D2D2FFC8C2BDFFECDBCAFFF1E5D8FFF6EF
    E7FFF6EFE7FFF1E6DAFFC8C3BEFFD2D2D2FFF8F8F8FFFAFAFAFF4C4C4C6A5050
    5068FDFDFDFFFCFCFCFFFCFCFCFFF7F7F7FFC6C6C6FFCCC6C1FFE8DBCDFFF5EB
    E3FFEDE6DFFFCCC7C2FFC6C6C6FFF7F7F7FFFCFCFCFFFDFDFDFF505050685454
    5467FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFAFFDBDBDBFFC4C4C4FFBBBB
    BBFFC4C4C4FFDBDBDBFFFAFAFAFFFFFFFFFFFFFFFFFFFFFFFFFF54545467D692
    3DE1EEB266FFEDB165FFEBAF63FFE9AD61FFE6AA5EFFE4A85CFFE1A559FFDEA2
    56FFDB9F53FFD89C50FFD69A4EFFD3974BFFD19549FFCF9347FFB06B14E0D391
    3FCDF4C375FDFED287FFFDCE85FFFACA83FFF8C57FFFF6C17BFFF4BD74FFF3BB
    6CFFF3BC60FFF4BE50FFF7C23EFFF9C82BFFFBCD19FFF2C020FDD3913FCDC28A
    455CD3953FCDD9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D998
    3EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D3953FCDC28A455C0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 603
    Height = 358
    inherited FGPanel: TPanel
      Width = 599
      Height = 354
      object pnlMain: TPanel
        Left = 0
        Top = 0
        Width = 599
        Height = 330
        Align = alClient
        BevelOuter = bvNone
        FullRepaint = False
        PopupMenu = TBXPopupMenu
        TabOrder = 0
        object Splitter: TSpTBXSplitter
          Left = 0
          Top = 239
          Width = 599
          Height = 3
          Cursor = crSizeNS
          Align = alBottom
          ParentColor = False
          MinSize = 20
        end
        object TBXDock1: TSpTBXDock
          Left = 0
          Top = 0
          Width = 599
          Height = 30
          AllowDrag = False
          DoubleBuffered = True
          object ToolBar: TSpTBXToolbar
            Left = 0
            Top = 0
            DockPos = 0
            FullSize = True
            Images = vilImages
            TabOrder = 0
            Customizable = False
            object tbiSearch: TSpTBXItem
              Action = actFileSearch
            end
            object tbiRefresh: TSpTBXItem
              Action = actFileRefresh
            end
            object TBXSeparatorItem1: TSpTBXSeparatorItem
            end
            object tbiAbort: TSpTBXItem
              Action = actFileAbort
            end
            object TBXSeparatorItem2: TSpTBXSeparatorItem
            end
            object tbiGoToSelected: TSpTBXItem
              Action = actListGotoSelected
            end
            object TBXSeparatorItem3: TSpTBXSeparatorItem
            end
            object tbiPrint: TSpTBXItem
              Action = actFilePrint
            end
            object tbiCopy: TSpTBXItem
              Action = actFileCopy
            end
            object tbiSave: TSpTBXItem
              Action = actFileSave
            end
            object TBXSeparatorItem4: TSpTBXSeparatorItem
            end
            object tbiExpand: TSpTBXItem
              Action = actListExpand
            end
            object tbiContract: TSpTBXItem
              Action = actListContract
            end
            object TBXSeparatorItem5: TSpTBXSeparatorItem
            end
            object tbiReplaceSelected: TSpTBXItem
              Action = actReplaceSelected
            end
            object tbiReplaceAll: TSpTBXItem
              Action = actReplaceAll
            end
            object TBXSeparatorItem6: TSpTBXSeparatorItem
            end
            object tbiOptions: TSpTBXItem
              Action = actViewOptions
            end
            object TBXSeparatorItem8: TSpTBXSeparatorItem
            end
            object tbiHelp: TSpTBXItem
              Action = actHelpHelp
            end
          end
        end
        object lbResults: TSpTBXListBox
          Left = 0
          Top = 30
          Width = 599
          Height = 209
          Align = alClient
          Constraints.MinHeight = 120
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 17
          ParentFont = False
          TabOrder = 0
          OnClick = lbResultsClick
          OnDblClick = actListGotoSelectedExecute
          OnDrawItem = lbResultsDrawItem
          OnKeyPress = lbResultsKeyPress
          OnMouseUp = lbResultsMouseUp
          HotTrack = False
        end
        object reContext: TSynEdit
          Left = 0
          Top = 242
          Width = 599
          Height = 88
          Cursor = crDefault
          Align = alBottom
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 1
          UseCodeFolding = False
          BorderStyle = bsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Gutter.Visible = False
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
            end
            item
              Kind = gbkMargin
              Width = 3
            end>
          HideSelection = True
          IndentGuides.Visible = False
          ReadOnly = True
          RightEdge = 0
          SelectedColor.Alpha = 0.400000005960464500
          OnSpecialLineColors = reContextSpecialLineColors
        end
      end
      object StatusBar: TSpTBXStatusBar
        Left = 0
        Top = 330
        Width = 599
        Height = 24
        SizeGrip = False
        object StatusLeftLabel: TSpTBXLabelItem
          Wrapping = twEndEllipsis
          MinHeight = 20
        end
        object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
          CustomWidth = 0
        end
        object StatusRightLabel: TSpTBXLabelItem
          Alignment = taRightJustify
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 22
    Top = 44
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 98
    Top = 94
    object mitFileSearch1: TSpTBXItem
      Action = actFileSearch
    end
    object mitFileRefresh1: TSpTBXItem
      Action = actFileRefresh
    end
    object mitFileAbort1: TSpTBXItem
      Action = actFileAbort
    end
    object N5: TSpTBXSeparatorItem
    end
    object mitFilePrint1: TSpTBXItem
      Action = actFilePrint
    end
    object mitFileSave1: TSpTBXItem
      Action = actFileSave
    end
    object N2: TSpTBXSeparatorItem
    end
    object mitViewToolBar1: TSpTBXItem
      Action = actViewToolBar
    end
    object StatusBar1: TSpTBXItem
      Action = actViewStatusBar
    end
    object miViewShowMatchContext1: TSpTBXItem
      Action = actViewShowContext
    end
    object N1: TSpTBXSeparatorItem
    end
    object mitReplaceReplaceAll1: TSpTBXItem
      Action = actReplaceAll
    end
    object mitReplaceSelected1: TSpTBXItem
      Action = actReplaceSelected
    end
    object N3: TSpTBXSeparatorItem
    end
    object mitViewOptions1: TSpTBXItem
      Action = actViewOptions
    end
    object N4: TSpTBXSeparatorItem
    end
    object mitHelp: TSpTBXItem
      Action = actHelpHelp
    end
  end
  object Actions: TActionList
    Images = vilImages
    OnUpdate = ActionsUpdate
    Left = 28
    Top = 95
    object actReplaceSelected: TAction
      Category = 'Replace'
      Caption = 'Replace Selected Item...'
      Hint = 'Replace selected item...'
      ImageIndex = 4
      ImageName = 'Replace'
      ShortCut = 24659
      OnExecute = actReplaceSelectedExecute
    end
    object actFileSearch: TAction
      Category = 'File'
      Caption = '&Search...'
      Hint = 'New search...'
      ImageIndex = 3
      ImageName = 'Search'
      ShortCut = 24646
      OnExecute = actFileSearchExecute
    end
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh search'
      ImageIndex = 12
      ImageName = 'FindRefresh'
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actFileAbort: TAction
      Category = 'File'
      Caption = '&Abort'
      Hint = 'Abort search'
      ImageIndex = 11
      ImageName = 'Abort'
      ShortCut = 16449
      OnExecute = actFileAbortExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print...'
      Hint = 'Print results...'
      ImageIndex = 1
      ImageName = 'Print'
      ShortCut = 16464
      OnExecute = actFilePrintExecute
    end
    object actListGotoSelected: TAction
      Category = 'List'
      Caption = 'Goto Selected'
      Hint = 'Goto match'
      ImageIndex = 9
      ImageName = 'GoToLine'
      OnExecute = actListGotoSelectedExecute
    end
    object actListContract: TAction
      Category = 'List'
      Caption = '&Contract'
      Hint = 'Contract all'
      ImageIndex = 8
      ImageName = 'Collapse'
      ShortCut = 16462
      OnExecute = actListContractExecute
    end
    object actListExpand: TAction
      Category = 'List'
      Caption = '&Expand'
      Hint = 'Expand all'
      ImageIndex = 7
      ImageName = 'Expand'
      ShortCut = 16453
      OnExecute = actListExpandExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 10
      ImageName = 'Help'
      OnExecute = actHelpHelpExecute
    end
    object actViewShowContext: TAction
      Category = 'View'
      Caption = 'Show Match Context'
      Checked = True
      Hint = 'View/Hide Context of found text'
      OnExecute = actViewShowContextExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save...'
      Hint = 'Save results to file...'
      ImageIndex = 0
      ImageName = 'Save'
      ShortCut = 49235
      OnExecute = actFileSaveExecute
    end
    object actFileCopy: TAction
      Category = 'File'
      Caption = '&Copy'
      Hint = 'Copy results to clipboard'
      ImageIndex = 2
      ImageName = 'Copy'
      ShortCut = 16451
      OnExecute = actFileCopyExecute
    end
    object actViewToolBar: TAction
      Category = 'View'
      Caption = 'Toolbar'
      Checked = True
      Hint = 'View/Hide Toolbar'
      OnExecute = actViewToolBarExecute
    end
    object actViewStatusBar: TAction
      Category = 'View'
      Caption = 'StatusBar'
      Checked = True
      Hint = 'View/Hide StatusBar'
      OnExecute = actViewStatusBarExecute
    end
    object actViewOptions: TAction
      Category = 'View'
      Caption = 'Options...'
      Hint = 'Configure Find in Files'
      ImageIndex = 6
      ImageName = 'Setup'
      OnExecute = actViewOptionsExecute
    end
    object actReplaceAll: TAction
      Category = 'Replace'
      Caption = 'Replace All Items...'
      Hint = 'Replace all items...'
      ImageIndex = 5
      ImageName = 'ReplaceAll'
      ShortCut = 24641
      OnExecute = actReplaceAllExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 99
        CollectionName = 'Save'
        Name = 'Save'
      end
      item
        CollectionIndex = 70
        CollectionName = 'Print'
        Name = 'Print'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 101
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 91
        CollectionName = 'Replace'
        Name = 'Replace'
      end
      item
        CollectionIndex = 92
        CollectionName = 'ReplaceAll'
        Name = 'ReplaceAll'
      end
      item
        CollectionIndex = 103
        CollectionName = 'Setup'
        Name = 'Setup'
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
        CollectionIndex = 0
        CollectionName = 'Abort'
        Name = 'Abort'
      end
      item
        CollectionIndex = 43
        CollectionName = 'FindRefresh'
        Name = 'FindRefresh'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 173
    Top = 96
  end
end
