inherited WatchesWindow: TWatchesWindow
  Left = 331
  Top = 325
  HelpContext = 490
  Caption = 'Watches'
  ClientHeight = 229
  ClientWidth = 760
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000A76F
    4EA9C28D67FFBF8A65FFBD8763FFBA8460FFB8825EFFB37D5BFFB17B59FFB07A
    57FFAD7856FFAC7555FFAA7453FFA87252FFA87050FFA76F4EA900000000C891
    6BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA87150FF00000000CA93
    6DFFC6B2A9FF6E4E32FF6E4E32FF6E4E32FFAB968BFFFEFEFCFFFEFEFCFFC6B2
    A9FF6E4E32FF6E4E32FF6E4E32FFC6B2A9FFFFFFFFFFA97252FF00000000CC96
    6EFF6E4E32FFECE8E4FFFFFFFCFFC6B2A9FF6E4E32FFFEFEFBFFFDFDFAFF6E4E
    32FFECE8E4FFFFFFFCFFC6B2A9FF6E4E32FFFFFFFFFFAB7453FF00000000D19B
    72FF6E4E32FFFFFAEBFFFFFFFCFFFFFFFCFF6E4E32FFFDFDFBFFFDFDFAFF6E4E
    32FFFFFAEBFFFFFFFCFFFFFFFCFF6E4E32FFFFFFFFFFAF7957FF00000000D49D
    74FF6E4E32FFFFF3D5FFFFFAEBFFFFFAEBFF6E4E32FFC6B2A9FFC6B2A9FF6E4E
    32FFFFF3D5FFFFFAEBFFFFFAEBFF6E4E32FFFFFFFFFFB17B59FF00000000D59F
    75FFC6B2A9FF6E4E32FF6E4E32FF6E4E32FFC6B2A9FF6E4E32FF6E4E32FFC6B2
    A9FF6E4E32FF6E4E32FF6E4E32FFAB968BFFFFFFFFFFB47D5BFF00000000D8A1
    78FFFFFFFFFF876C55FFC6B2A9FFFCFBF9FFFBFAF6FFFBF8F5FFFBF7F4FFFBF6
    F1FFF8F4EEFFF7F2EBFFC6B2A9FF6E4E32FFFFFFFFFFB6805DFF00000000D9A2
    78FFFFFFFFFFFCFBF9FF6E4E32FFC6B2A9FFFBF7F4FFFAF7F2FF6E4E32FFF7F3
    EDFFF6EFEAFFF5EBE7FFF3EAE4FFC6B2A9FF6E4E32FFB9845FFF00000000DBA3
    79FFFFFFFFFFFFFFFFFFFFFFFFFF6E4E32FFC6B2A9FFFFFFFFFF876C55FFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6B2A9FF6E4E32FF00000000DCA6
    7AFFDCA67AFFDCA67AFFDCA67AFFDCA67AFFDCA67AFFDCA67AFFDCA67AFFDCA6
    7AFFDCA67AFFDCA67AFFDCA67AFFDCA67AFFDCA67AFFBF8A65FF00000000D8AB
    84FDE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
    91FFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFBF8F69FD00000000A670
    516BDCAD8CF4DCA67AFFDCA579FFDAA379FFD8A178FFD59F75FFD49D74FFD29C
    72FFCF9971FFCE986FFFCB956EFFC9936BFFC39679F4A670516B000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited BGPanel: TPanel
    Width = 760
    Height = 229
    inherited FGPanel: TPanel
      Width = 756
      Height = 225
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 756
        Height = 225
        Align = alClient
        TabOrder = 0
        object WatchesView: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 754
          Height = 223
          Align = alClient
          Alignment = taRightJustify
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = -1
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
          HintMode = hmTooltip
          Images = vilCodeImages
          PopupMenu = TBXPopupMenu
          TabOrder = 0
          TreeOptions.AnimationOptions = [toAnimatedToggle]
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnDblClick = WatchesViewDblClick
          OnDragOver = WatchesViewDragOver
          OnDragDrop = WatchesViewDragDrop
          OnFreeNode = WatchesViewFreeNode
          OnGetText = WatchesViewGetText
          OnGetImageIndex = WatchesViewGetImageIndex
          OnInitChildren = WatchesViewInitChildren
          OnInitNode = WatchesViewInitNode
          OnKeyDown = WatchesViewKeyDown
          Columns = <
            item
              Position = 0
              Text = 'Watches'
              Width = 200
            end
            item
              Position = 1
              Text = 'Type'
              Width = 120
            end
            item
              Position = 2
              Text = 'Value'
              Width = 434
            end>
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 24
    Top = 26
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    OnPopup = TBXPopupMenuPopup
    Left = 24
    Top = 85
    object mnAddWatch: TSpTBXItem
      Caption = '&Add Watch'
      ImageIndex = 2
      OnClick = mnAddWatchClick
    end
    object TBXItem1: TSpTBXItem
      Action = PyIDEMainForm.actAddWatchAtCursor
      ImageIndex = 4
    end
    object mnRemoveWatch: TSpTBXItem
      Caption = '&Remove Watch'
      ImageIndex = 3
      OnClick = mnRemoveWatchClick
    end
    object mnEditWatch: TSpTBXItem
      Caption = '&Edit Watch'
      ImageIndex = 5
      OnClick = mnEditWatchClick
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClearall: TSpTBXItem
      Caption = '&Clear all'
      Hint = 'Clear all watches'
      ImageIndex = 1
      OnClick = mnClearAllClick
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnCopyToClipboard: TSpTBXItem
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 0
      OnClick = mnCopyToClipboardClick
    end
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
    Left = 24
    Top = 136
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 12
        CollectionName = 'Item13'
        Disabled = False
        Name = 'Item13'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Item15'
        Disabled = False
        Name = 'Item15'
      end
      item
        CollectionIndex = 49
        CollectionName = 'Item50'
        Disabled = False
        Name = 'Item50'
      end
      item
        CollectionIndex = 50
        CollectionName = 'Item51'
        Disabled = False
        Name = 'Item51'
      end
      item
        CollectionIndex = 66
        CollectionName = 'Item67'
        Disabled = False
        Name = 'Item67'
      end
      item
        CollectionIndex = 92
        CollectionName = 'Item93'
        Disabled = False
        Name = 'Item93'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 88
    Top = 136
  end
end
