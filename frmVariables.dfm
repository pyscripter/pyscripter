inherited VariablesWindow: TVariablesWindow
  Left = 237
  Top = 237
  HelpContext = 480
  Caption = 'Variables'
  ClientHeight = 247
  ClientWidth = 579
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040000000000000000000000000000000000000000000
    0000000000000000000000000000848484FF848484FF848484FF848484FF8484
    84FF848484FF848484FF848484FF000000000000000000000000000000000000
    0000000000000000000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000000000
    0000000000000000000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FF848484FF00000000000000000000
    0000000000000000000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FF00000000000000000000
    0000000000000000000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FFFFFFFFFF848484FF0000
    000000000000000000FF00000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FFFFFFFFFF848484FF0000
    0000000000000000000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FFFFFFFFFF848484FF0000
    000000000000000000FF00000000848484FF848484FF848484FF848484FF8484
    84FF848484FF848484FF848484FFFFFFFFFF848484FFFFFFFFFF848484FF0000
    00000000000000000000000000000000000000000000848484FFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FFFFFFFFFF848484FF0000
    000000000000000000FF000000000000000000000000848484FF848484FF8484
    84FF848484FF848484FF848484FF848484FF848484FFFFFFFFFF848484FF0000
    0000000000000000000000000000000000000000000000000000000000008484
    84FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FF0000
    0000848484FF840000FF840000FF000000000000000000000000000000008484
    84FF848484FF848484FF848484FF848484FF848484FF848484FF848484FFFF00
    00FFFF0000FF840000FF840000FF840000FF0000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FF00
    00FFFF0000FFFF0000FF840000FF840000FF00000000000000FF000000000000
    00FF00000000000000FF00000000000000000000000000000000000000008484
    84FFFF0000FFFF0000FFFF0000FF848484FF0000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000848484FFFF0000FFFF0000FF000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000F00F
    0000F00F0000F0030000F0030000F0000000D0000000F0000000D0000000FC00
    0000DC000000FF0000008F00000007FF0000055F000007FF00008FFF0000}
  OnActivate = FormActivate
  ExplicitWidth = 587
  ExplicitHeight = 273
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Left = 5
    Top = 11
    Width = 569
    Height = 213
    ExplicitLeft = 5
    ExplicitTop = 11
    ExplicitWidth = 569
    ExplicitHeight = 213
    object Splitter: TJvNetscapeSplitter
      Left = 184
      Top = 0
      Height = 213
      Align = alRight
      MinSize = 10
      ResizeStyle = rsUpdate
      Maximized = False
      Minimized = False
      ButtonCursor = crDefault
      ButtonHighlightColor = 14400935
      AutoHighlightColor = True
    end
    object VariablesTree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 184
      Height = 213
      Align = alClient
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Header.AutoSizeIndex = 2
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoHotTrack, hoOwnerDraw, hoVisible]
      Header.PopupMenu = VTHeaderPopupMenu
      HintMode = hmTooltip
      Images = CommandsDataModule.CodeImages
      IncrementalSearch = isVisibleOnly
      TabOrder = 0
      TreeOptions.AnimationOptions = [toAnimatedToggle]
      TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toVariableNodeHeight]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnChange = VariablesTreeChange
      OnGetText = VariablesTreeGetText
      OnPaintText = VariablesTreePaintText
      OnGetImageIndex = VariablesTreeGetImageIndex
      OnInitNode = VariablesTreeInitNode
      Columns = <
        item
          Position = 0
          Width = 160
          WideText = 'Name'
        end
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
          Position = 1
          Width = 100
          WideText = 'Type'
        end
        item
          Position = 2
          Width = 24
          WideText = 'Value'
        end>
    end
    object TBXPageScroller: TTBXPageScroller
      Left = 194
      Top = 0
      Width = 375
      Height = 213
      Align = alRight
      AutoRange = True
      Color = clBtnFace
      DoubleBuffered = False
      ParentColor = False
      TabOrder = 1
      object HTMLLabel: TJvLinkLabel
        Left = 0
        Top = 0
        Width = 375
        Caption = '<b>NameSpace'
        Text.Strings = (
          '<b>NameSpace')
        MarginWidth = 2
        MarginHeight = 2
        Align = alTop
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
  end
  inherited DockClient: TJvDockClient
    LeftDock = False
    RightDock = False
  end
  object VTHeaderPopupMenu: TVTHeaderPopupMenu
    Left = 44
    Top = 35
  end
end
