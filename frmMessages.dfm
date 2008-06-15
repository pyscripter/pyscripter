inherited MessagesWindow: TMessagesWindow
  Left = 259
  Top = 257
  HelpContext = 440
  Caption = 'Messages'
  ClientHeight = 186
  ClientWidth = 693
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000FF000000FF000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000FFC6C6C6FF000000FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000FFFFFFFFFFC6C6C6FF0000
    00FF000000000000000000000000000000000000000000000000000000000000
    000000000000000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFC6C6
    C6FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    000000000000848484FFC6C6C6FFC6C6C6FFC6C6C6FFFFFFFFFFFFFFFFFFFFFF
    FFFFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6FF000000FF0000
    000000000000848484FF848484FF848484FF848484FF848484FF848484FF8484
    84FF848484FF848484FF848484FF848484FF848484FF848484FF848484FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000F9FF
    0000F8FF0000F87F0000C0000000C0000000C0000000C0000000C0000000C000
    0000C0000000C0000000C0000000C0000000C0000000FFFF0000FFFF0000}
  ExplicitWidth = 709
  ExplicitHeight = 220
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 687
    Height = 180
    ExplicitLeft = 3
    ExplicitTop = 3
    ExplicitWidth = 687
    ExplicitHeight = 180
    DesignSize = (
      687
      180)
    object MessagesView: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 687
      Height = 180
      Align = alClient
      Alignment = taRightJustify
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
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
      OnDblClick = MessagesViewDblClick
      OnGetText = MessagesViewGetText
      OnInitNode = MessagesViewInitNode
      Columns = <
        item
          Position = 0
          Width = 337
          WideText = 'Message'
        end
        item
          Position = 1
          Width = 200
          WideText = 'File Name'
        end
        item
          Alignment = taRightJustify
          Position = 2
          WideText = 'Line'
        end
        item
          Alignment = taRightJustify
          Position = 3
          Width = 60
          WideText = 'Position'
        end
        item
          Position = 4
          Width = 40
        end>
    end
    object TBToolbar1: TTBToolbar
      Left = 635
      Top = 0
      Width = 39
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'TBToolbar1'
      TabOrder = 1
      DesignSize = (
        39
        17)
      object TBControlItem5: TTBControlItem
        Control = BtnPreviousMsgs
      end
      object TBControlItem6: TTBControlItem
        Control = BtnNextMsgs
      end
      object BtnPreviousMsgs: TSpTBXButton
        Left = 0
        Top = 0
        Width = 20
        Height = 17
        Hint = 'Show previous messages'
        Caption = '3'
        Anchors = [akTop, akRight]
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = actPreviousMsgsExecute
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
        ThemeType = thtTBX
      end
      object BtnNextMsgs: TSpTBXButton
        Left = 20
        Top = 0
        Width = 19
        Height = 17
        Hint = 'Show next messages'
        Caption = '4'
        Anchors = [akTop, akRight]
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = actNextMsgsExecute
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
        ThemeType = thtTBX
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 6
    Top = 12
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    OnPopup = TBXPopupMenuPopup
    Left = 10
    Top = 82
    object mnPreviousMessage: TSpTBXItem
      Caption = '&Previous Messages'
      Hint = 'Show previous messages'
      Action = actPreviousMsgs
    end
    object mnNextMessage: TSpTBXItem
      Caption = '&Next Messages'
      Hint = 'Show next messages'
      Action = actNextMsgs
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClearall: TSpTBXItem
      Caption = '&Clear all'
      Hint = 'Clear all messages'
      Action = actClearAll
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnCopy: TSpTBXItem
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy contents to Clipboard'
      Action = actCopyToClipboard
    end
  end
  object MsgsActionList: TTntActionList
    Images = CommandsDataModule.Images
    Left = 9
    Top = 47
    object actClearAll: TTntAction
      Caption = '&Clear all'
      Hint = 'Clear all messages'
      ImageIndex = 14
      OnExecute = ClearAllExecute
    end
    object actPreviousMsgs: TTntAction
      Caption = '&Previous Messages'
      Hint = 'Show previous messages'
      ImageIndex = 96
      OnExecute = actPreviousMsgsExecute
    end
    object actNextMsgs: TTntAction
      Caption = '&Next Messages'
      Hint = 'Show next messages'
      ImageIndex = 97
      OnExecute = actNextMsgsExecute
    end
    object actCopyToClipboard: TTntAction
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy contents to Clipboard'
      ImageIndex = 12
      OnExecute = actCopyToClipboardExecute
    end
  end
end
