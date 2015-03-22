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
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 392
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 599
    Height = 354
    ExplicitLeft = 2
    ExplicitTop = 2
    ExplicitWidth = 599
    ExplicitHeight = 354
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
        Top = 234
        Width = 599
        Height = 8
        Cursor = crSizeNS
        Align = alBottom
        MinSize = 20
      end
      object TBXDock1: TSpTBXDock
        Left = 0
        Top = 0
        Width = 599
        Height = 26
        AllowDrag = False
        object ToolBar: TSpTBXToolbar
          Left = 0
          Top = 0
          DockPos = 0
          FullSize = True
          Images = CommandsDataModule.Images
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
        Top = 26
        Width = 599
        Height = 208
        Align = alClient
        Constraints.MinHeight = 120
        ItemHeight = 17
        TabOrder = 0
        OnClick = lbResultsClick
        OnDblClick = actListGotoSelectedExecute
        OnDrawItem = lbResultsDrawItem
        OnKeyPress = lbResultsKeyPress
        OnMouseUp = lbResultsMouseUp
        HotTrack = False
      end
      object reContext: TRichEdit
        Left = 0
        Top = 242
        Width = 599
        Height = 88
        Align = alBottom
        Font.Charset = GREEK_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
        Zoom = 100
      end
    end
    object StatusBar: TSpTBXStatusBar
      Left = 0
      Top = 330
      Width = 599
      Height = 24
      object StatusLeftLabel: TSpTBXLabelItem
        Wrapping = twEndEllipsis
        MinHeight = 20
      end
      object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
        CustomWidth = 570
      end
      object StatusRightLabel: TSpTBXLabelItem
        Alignment = taRightJustify
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 22
    Top = 44
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 26
    Top = 118
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
    Images = CommandsDataModule.Images
    OnUpdate = ActionsUpdate
    Left = 24
    Top = 80
    object actReplaceSelected: TAction
      Category = 'Replace'
      Caption = 'Replace Selected Item...'
      Hint = 'Replace selected item...'
      ImageIndex = 17
      ShortCut = 24659
      OnExecute = actReplaceSelectedExecute
    end
    object actFileSearch: TAction
      Category = 'File'
      Caption = '&Search...'
      Hint = 'New search...'
      ImageIndex = 15
      ShortCut = 24646
      OnExecute = actFileSearchExecute
    end
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh search'
      ImageIndex = 42
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actFileAbort: TAction
      Category = 'File'
      Caption = '&Abort'
      Hint = 'Abort search'
      ImageIndex = 40
      ShortCut = 16449
      OnExecute = actFileAbortExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print...'
      Hint = 'Print results...'
      ImageIndex = 8
      ShortCut = 16464
      OnExecute = actFilePrintExecute
    end
    object actListGotoSelected: TAction
      Category = 'List'
      Caption = 'Goto Selected'
      Hint = 'Goto match'
      ImageIndex = 32
      OnExecute = actListGotoSelectedExecute
    end
    object actListContract: TAction
      Category = 'List'
      Caption = '&Contract'
      Hint = 'Contract all'
      ImageIndex = 29
      ShortCut = 16462
      OnExecute = actListContractExecute
    end
    object actListExpand: TAction
      Category = 'List'
      Caption = '&Expand'
      Hint = 'Expand all'
      ImageIndex = 28
      ShortCut = 16453
      OnExecute = actListExpandExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 33
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
      ImageIndex = 4
      ShortCut = 49235
      OnExecute = actFileSaveExecute
    end
    object actFileCopy: TAction
      Category = 'File'
      Caption = '&Copy'
      Hint = 'Copy results to clipboard'
      ImageIndex = 12
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
      ImageIndex = 24
      OnExecute = actViewOptionsExecute
    end
    object actReplaceAll: TAction
      Category = 'Replace'
      Caption = 'Replace All Items...'
      Hint = 'Replace all items...'
      ImageIndex = 18
      ShortCut = 24641
      OnExecute = actReplaceAllExecute
    end
  end
end
