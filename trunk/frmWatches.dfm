inherited WatchesWindow: TWatchesWindow
  Left = 331
  Top = 325
  HelpContext = 490
  Caption = 'Watches'
  ClientHeight = 225
  ClientWidth = 439
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000FF000000FF00000000000000000000000000000000000000FF0000
    00FF000000000000000000000000000000000000000000000000000000000000
    00FFC0C0C0FFC0C0C0FF000000FF0000000000000000000000FFC0C0C0FFC0C0
    C0FF000000FF0000000000000000000000000000000000000000000000000000
    00FFFFFF00FFFFFFFFFF000000FF0000000000000000000000FFFFFF00FFFFFF
    FFFF000000FF0000000000000000000000000000000000000000000000000000
    00FFFFFF00FFFFFF00FF000000FF000000FF000000FF000000FFFFFF00FFFFFF
    00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    0000000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    00FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    000000000000000000FF000000FFC0C0C0FFC0C0C0FFC0C0C0FFFFFFFFFFFFFF
    FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF0000
    000000000000000000FFC0C0C0FF000000FFFFFFFFFFFFFFFFFF000000FFFFFF
    FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FFFFFFFFFF000000FF0000
    000000000000000000FFFFFFFFFFC0C0C0FF000000FFC0C0C0FF000000FFC0C0
    C0FF808080FFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFF000000FF0000
    000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    000000000000000000FFFFFFFFFF808080FF808080FF808080FFFFFFFFFF8080
    80FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    000000000000000000FFFFFFFFFF808080FF808080FF808080FF808080FFFFFF
    FFFF808080FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
    000000000000800000FF800000FF800000FF800000FF800000FF800000FF8000
    00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF0000
    000000000000800000FF800000FF800000FF800000FF800000FF800000FF8000
    00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FFFFFF
    00009E7F00000C3F00000C3F00000000000080000000C0000000C0000000C000
    0000C0000000C0000000C0000000C0000000C0000000C0000000C0000000}
  ExplicitWidth = 455
  ExplicitHeight = 259
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 433
    Height = 219
    ExplicitLeft = 3
    ExplicitTop = 3
    ExplicitWidth = 433
    ExplicitHeight = 219
    object WatchesView: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 433
      Height = 219
      Align = alClient
      Alignment = taRightJustify
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
      Header.MainColumn = 1
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoOwnerDraw, hoVisible]
      Header.ParentFont = True
      HintMode = hmTooltip
      PopupMenu = TBXPopupMenu
      TabOrder = 0
      TreeOptions.AnimationOptions = [toAnimatedToggle]
      TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnDblClick = WatchesViewDblClick
      OnGetText = WatchesViewGetText
      OnInitNode = WatchesViewInitNode
      OnKeyDown = WatchesViewKeyDown
      Columns = <
        item
          Position = 0
          Width = 200
          WideText = 'Watches'
        end
        item
          Position = 1
          Width = 233
          WideText = 'Value'
        end>
    end
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    OnPopup = TBXPopupMenuPopup
    Left = 8
    Top = 45
    object mnAddWatch: TSpTBXItem
      Caption = '&Add Watch'
      ImageIndex = 49
      OnClick = mnAddWatchClick
    end
    object TBXItem1: TSpTBXItem
      Caption = 'Add &Watch At Cursor'
      Hint = 'Add the expression at the editor current position as a watch'
      Action = PyIDEMainForm.actAddWatchAtCursor
    end
    object mnRemoveWatch: TSpTBXItem
      Caption = '&Remove Watch'
      ImageIndex = 50
      OnClick = mnRemoveWatchClick
    end
    object mnEditWatch: TSpTBXItem
      Caption = '&Edit Watch'
      ImageIndex = 92
      OnClick = mnEditWatchClick
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClearall: TSpTBXItem
      Caption = '&Clear all'
      Hint = 'Clear all watches'
      ImageIndex = 14
      OnClick = mnClearAllClick
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnCopyToClipboard: TSpTBXItem
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 12
      OnClick = mnCopyToClipboardClick
    end
  end
end
