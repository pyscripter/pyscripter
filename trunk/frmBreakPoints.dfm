inherited BreakPointsWindow: TBreakPointsWindow
  Left = 491
  Top = 381
  HelpContext = 495
  Caption = 'Breakpoints'
  ClientHeight = 244
  ClientWidth = 379
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000840000FF840000FF840000FF8400
    00FF840000FF840000FF840000FF840000FF0000000000000000000000000000
    000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000000000000000000000000000
    0000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF00000000000000000000
    00000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF00000000000000000000
    00000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF00000000000000000000
    000000000000000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF00000000000000000000
    0000000000FFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFF000000FFFFFF
    FFFF000000FFFFFFFFFF000000FFFFFFFFFF000000FF00000000000000000000
    0000000000FF000000FF000000FF00000000000000FFFFFFFFFF000000FFFFFF
    FFFF000000FFFFFFFFFF000000FFFFFFFFFF000000FF00000000000000000000
    000000000000000000000000000000000000000000FFFFFFFFFF000000FFFFFF
    FFFF000000FFFFFFFFFF000000FFFFFFFFFF000000FF00000000000000000000
    000000000000000000000000000000000000000000FFFFFFFFFF000000FFFFFF
    FFFF000000FFFFFFFFFF000000FFFFFFFFFF000000FF00000000000000000000
    000000000000000000000000000000000000000000FFFFFFFFFF000000FFFFFF
    FFFF000000FFFFFFFFFF000000FF000000FF0000000000000000000000000000
    000000000000000000000000000000000000000000FFFFFFFFFF000000FFFFFF
    FFFF000000FFFFFFFFFF000000FF000000000000000000000000000000000000
    00000000000000000000000000000000000000000000000000FF000000FFFFFF
    FFFF000000FF000000FF00000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000FF0000
    00FF00000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000F8070000F8070000F0030000E0030000E0030000C00300008003
    000088030000F8030000F8030000F8070000F80F0000FC1F0000FE7F0000}
  ExplicitWidth = 395
  ExplicitHeight = 278
  DesignSize = (
    379
    244)
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 370
    Height = 230
    ExplicitWidth = 370
    ExplicitHeight = 230
    object BreakPointsView: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 370
      Height = 230
      Align = alClient
      Alignment = taRightJustify
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      CheckImageKind = ckXP
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoHotTrack, hoOwnerDraw, hoVisible]
      Header.ParentFont = True
      HintMode = hmTooltip
      PopupMenu = TBXPopupMenu
      TabOrder = 0
      TreeOptions.AnimationOptions = [toAnimatedToggle]
      TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnChecked = BreakPointsViewChecked
      OnDblClick = BreakPointLVDblClick
      OnGetText = BreakPointsViewGetText
      OnInitNode = BreakPointsViewInitNode
      Columns = <
        item
          Position = 0
          Width = 300
          WideText = 'File Name'
        end
        item
          Alignment = taRightJustify
          Position = 1
          WideText = 'Line'
        end
        item
          Position = 2
          Width = 200
          WideText = 'Condition'
        end>
    end
  end
  inherited DockClient: TJvDockClient
    Left = 12
    Top = 18
  end
  object TBXPopupMenu: TTBXPopupMenu
    Images = CommandsDataModule.Images
    OnPopup = TBXPopupMenuPopup
    Left = 13
    Top = 50
    object mnSetCondition: TTBXItem
      Caption = 'Set &Condition...'
      OnClick = mnSetConditionClick
    end
    object TBXSeparatorItem1: TTBXSeparatorItem
    end
    object mnClear: TTBXItem
      Caption = 'C&lear'
      Hint = 'Clear|Clear selected breakpoint'
      OnClick = mnClearClick
    end
    object Breakpoints1: TTBXItem
      Action = PyIDEMainForm.actClearAllBreakpoints
    end
    object TBXSeparatorItem2: TTBXSeparatorItem
    end
    object mnCopyToClipboard: TTBXItem
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 12
      OnClick = mnCopyToClipboardClick
    end
  end
end
