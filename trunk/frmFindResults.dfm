inherited FindResultsWindow: TFindResultsWindow
  Left = 362
  Top = 146
  HelpContext = 460
  Caption = 'Find in Files Results'
  ClientHeight = 339
  ClientWidth = 603
  Font.Name = 'Tahoma'
  Icon.Data = {
    0000010001001010000000000000680500001600000028000000100000002000
    0000010008000000000040010000000000000000000000010000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C000C0DC
    C000F0CAA600F0F0F00099FFFF0099D49900FFD49900FFCCFF009999FF003022
    2200110000002200000044000000550000007700000088000000AA000000DD00
    0000EE00000000110000002200000044000000550000007700000088000000AA
    000000DD000000EE000000001100000022000000440000005500000077000000
    90000000AA000000DD000000EE00330000006600000099000000CC0000000033
    00003333000066330000A1330000CC330000FF33000000660000336600006666
    000099660000CC660000FF66000000990000339900006699000099990000CC99
    0000FF99000000CC000033CC000066CC000099CC0000CCCC0000FFCC000033FF
    000066FF000099FF0000CCFF000000003300330033006600330099003300CC00
    3300FF003300003333003B3333006633330099333300CC333300FF3333000066
    3300336E33006666330099663300CC663300FF66330000993300339933006699
    330099993300CC993300FF99330000CC330033CC330066CC330099CC3300CCCC
    3300FFCC330000FF330033FF330066FF330099FF3300CCFF3300FFFF33000000
    6600330066006600660099006600CC006600FF00660000336600333366006633
    660099336600CC336600FF33660000666600336666006666660099666600CC66
    6600FF66660000996600339966006699660099996600CC996600FF99660000CC
    660033CC660066CC660099CC6600CCCC6600FFCC660000FF660033FF660066FF
    660099FF6600CCFF6600FFFF660000009900330099006600990099009900CC00
    9900FF00990000339900333399006633990099339900CC339900FF3399000066
    A100336699006666990099669900CC669900FF66990000999900339999006699
    990099999900CC999900FF99990000CC990033CC990066CC990099CC9900CCCC
    9900FFCC990000FF990033FF990066FF990099FF9900CCFF9900FFFF99000000
    CC003300CC006600CC009900CC00CC00CC00FF08D4000033CC003333CC006633
    CC009933CC00CC33CC00FF33CC000066CC003366CC006666CC009966CC00CC66
    CC00FF66CC000099CC003399CC006699CC009999CC00CC99CC00FF99CC0000CC
    CC0033CCCC0066CCCC0099CCCC00CCCCCC00FFCCCC0000FFCC0033FFCC0066FF
    CC0099FFCC00CCFFCC00FFFFCC003300FF006600FF009900FF00CC00FF000033
    FF003333FF006633FF009933FF00CC33FF00FF33FF000066FF003366FF006666
    FF009966FF00CC66FF00FF66FF000099FF00DDDDDD00CC99FF0066CCFF000000
    8800FF00CC0099330000336633000066990033333300F0FBFF00A4A0A0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000A9
    547EA2A277A900000000000000007ED5AA14A80FB952F8000000000000007E09
    AB1001C6BF700710A2AF000000007E0E07A8EFC4C6260AD57F787E080000075A
    A9EFD0EC2610F8D4AAAA777E0800780710EF0BCAA8D4077E5A7FAA7785005A7F
    A9D2D7CA537E787EF8F87FF5A20077FF7EAA7DA9ABABCFABA9F8F8F57E00F8A9
    CFAAD5CFCF09ABABA410A953CD00F8D4CDCFAACEAAA3807F5A54A97D0000F7D4
    07A3CEAAAA807F5478F77E00000000F7A9F6CDA9F8A8F8F7A97D540700000000
    00F8F8F8A27E7E84F7077D7ED300000000000000000000CDF87E53007E070000
    0000000000000000F77E8477007D0000000000000000000000F77E7E100080FF
    0000007F0000000F000000030000000100000001000000010000000100000001
    0000000300000007000080030000E0010000FF800000FFC00000FFE00000}
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 373
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Left = 4
    Top = 6
    Width = 595
    Height = 320
    ExplicitLeft = 4
    ExplicitTop = 6
    ExplicitWidth = 595
    ExplicitHeight = 320
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 595
      Height = 298
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      PopupMenu = TBXPopupMenu
      TabOrder = 0
      object Splitter: TSplitter
        Left = 0
        Top = 202
        Width = 595
        Height = 8
        Cursor = crVSplit
        Align = alBottom
        AutoSnap = False
        Beveled = True
        MinSize = 20
        ResizeStyle = rsUpdate
      end
      object lbResults: TListBox
        Left = 0
        Top = 26
        Width = 595
        Height = 176
        Style = lbOwnerDrawFixed
        Align = alClient
        Constraints.MinHeight = 120
        ItemHeight = 17
        TabOrder = 0
        OnClick = lbResultsClick
        OnDblClick = actListGotoSelectedExecute
        OnDrawItem = lbResultsDrawItem
        OnKeyPress = lbResultsKeyPress
        OnMouseUp = lbResultsMouseUp
      end
      object reContext: TRichEdit
        Left = 0
        Top = 210
        Width = 595
        Height = 88
        Align = alBottom
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object TBXDock1: TTBXDock
        Left = 0
        Top = 0
        Width = 595
        Height = 26
        AllowDrag = False
        object ToolBar: TTBXToolbar
          Left = 0
          Top = 0
          Caption = 'ToolBar'
          FullSize = True
          Images = CommandsDataModule.Images
          TabOrder = 0
          object TBXItem2: TTBXItem
            Action = actFileSearch
          end
          object TBXItem1: TTBXItem
            Action = actFileRefresh
          end
          object TBXSeparatorItem1: TTBXSeparatorItem
          end
          object TBXItem3: TTBXItem
            Action = actFileAbort
          end
          object TBXSeparatorItem2: TTBXSeparatorItem
          end
          object TBXItem4: TTBXItem
            Action = actListGotoSelected
          end
          object TBXSeparatorItem3: TTBXSeparatorItem
          end
          object TBXItem7: TTBXItem
            Action = actFilePrint
          end
          object TBXItem6: TTBXItem
            Action = actFileCopy
          end
          object TBXItem5: TTBXItem
            Action = actFileSave
          end
          object TBXSeparatorItem4: TTBXSeparatorItem
          end
          object TBXItem9: TTBXItem
            Action = actListExpand
          end
          object TBXItem8: TTBXItem
            Action = actListContract
          end
          object TBXSeparatorItem5: TTBXSeparatorItem
          end
          object TBXItem11: TTBXItem
            Action = actReplaceSelected
          end
          object TBXItem10: TTBXItem
            Action = actReplaceAll
          end
          object TBXSeparatorItem6: TTBXSeparatorItem
          end
          object TBXItem13: TTBXItem
            Action = actViewOptions
          end
          object TBXSeparatorItem7: TTBXSeparatorItem
          end
          object TBXItem12: TTBXItem
            Action = actViewStayOnTop
          end
          object TBXSeparatorItem8: TTBXSeparatorItem
          end
          object TBXItem14: TTBXItem
            Action = actHelpHelp
          end
        end
      end
    end
    object StatusBar: TTBXStatusBar
      Left = 0
      Top = 298
      Width = 595
      Panels = <
        item
          ViewPriority = 80
          StretchPriority = 100
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Size = 70
          Tag = 0
        end>
      UseSystemFont = False
    end
  end
  inherited DockClient: TJvDockClient
    Left = 22
    Top = 44
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
    object actViewStayOnTop: TAction
      Category = 'View'
      Caption = 'Stay on Top'
      Hint = 'Stay on top'
      ImageIndex = 41
      OnExecute = actViewStayOnTopExecute
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
  object TBXPopupMenu: TTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 26
    Top = 118
    object mitFileSearch1: TTBXItem
      Action = actFileSearch
    end
    object mitFileRefresh1: TTBXItem
      Action = actFileRefresh
    end
    object mitFileAbort1: TTBXItem
      Action = actFileAbort
    end
    object N5: TTBXSeparatorItem
    end
    object mitFilePrint1: TTBXItem
      Action = actFilePrint
    end
    object mitFileSave1: TTBXItem
      Action = actFileSave
    end
    object N2: TTBXSeparatorItem
    end
    object mitViewToolBar1: TTBXItem
      Action = actViewToolBar
    end
    object StatusBar1: TTBXItem
      Action = actViewStatusBar
    end
    object miViewShowMatchContext1: TTBXItem
      Action = actViewShowContext
    end
    object N1: TTBXSeparatorItem
    end
    object mitReplaceReplaceAll1: TTBXItem
      Action = actReplaceAll
    end
    object mitReplaceSelected1: TTBXItem
      Action = actReplaceSelected
    end
    object N3: TTBXSeparatorItem
    end
    object mitViewOptions1: TTBXItem
      Action = actViewOptions
    end
    object N4: TTBXSeparatorItem
    end
    object mitViewStayOnTop1: TTBXItem
      Action = actViewStayOnTop
    end
    object TBXSeparatorItem9: TTBXSeparatorItem
    end
    object TBXItem15: TTBXItem
      Action = actHelpHelp
    end
  end
end
