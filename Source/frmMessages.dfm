inherited MessagesWindow: TMessagesWindow
  Left = 259
  Top = 257
  HelpContext = 440
  ActiveControl = MessagesView
  Caption = 'Messages'
  ClientHeight = 218
  ClientWidth = 741
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000B6885B1CB88556AEB985
    562C000000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000BF7F550CB88755F1B587
    55F7B78456320000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000B88758D9E2CE
    BAFFB88458F8B687583100000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000B7875755B88755F1F9F6
    F1FFE2CDB9FFB58458FEB88658DCB68758D3B8875691BB88550F000000000000
    0000B6855515B7865881B983552100000000B7865887CFAE8EFFEBDED1FFFDFC
    FAFFFDFCFAFFFDFBF8FFF4EBE3FFF1E7DDFFD8BEA3FFB88458F6B78657720000
    0000AA8D5509B68758B3B68756B7BA895925B68755E8F8F1ECFFFDFBF8FFF8F0
    E7FFF7EFE6FFF7EEE4FFF8F0E8FFF8F1EAFFFDFBF9FFF2E9E0FFB88658D80000
    000000000000B78658A1E2CEBABDB88657B8B58758F3FAF7F2FFFAF3ECFFF8F1
    E9FFF8F0E7FFF7EFE6FFF7EEE4FFF6ECE2FFF7EFE7FFF4ECE5FFB88458DF0000
    0000B688593CB88758AAF5F0E9BDE2CDB7BDB58758FBFAF6F1FFFAF4EEFFF9F2
    EAFFF8F1E9FFF8F0E8FFF7EFE6FFF7EEE4FFF8F1E9FFF3EAE2FFB68656DAB787
    5964CEAB8CBDE9DACEBDFDFCF9BDFDFCF9BDB78B5DFBFAF6F2FFFAF5EFFFFAF4
    EEFFFAF3EDFFF9F3EBFFF9F2EBFFF8F1E9FFFAF5EFFFF3EAE2FFB68656DAB685
    57ACF8F1ECBDFDF9F9BDF8F0E5BDF5EDE5BDC39C72F0E4D1BEFFFAF6F2FFFAF6
    F1FFFAF6F1FFFAF5F1FFFAF5F1FFFAF5F0FFF6F0E9FFDCC4ADFFB88758ADB687
    58B3F9F5F1BDF9F1ECBDF8F1E9BDF8F0E5BDE4D3C0CBC49B70EEB88C5EFBB88C
    5FFBB88C5EFBB88758F9B78757EFB78757EFB88758E4B8875786B8855928B786
    57B2F9F5F1BDF9F4EDBDF9F1E9BDF8F1E9BDF8F0E8BDF5EDE5BDF5EDE4BDF8F1
    E9BDF1E9E2BDB88758A20000000000000000000000000000000000000000B786
    57B2F9F5F1BDF9F5EDBDF9F4EDBDF9F1EDBDF9F1E9BDF9F1E9BDF8F1E9BDF9F5
    EDBDF1E9E2BDB88758A20000000000000000000000000000000000000000B887
    5691E4D1BEBDF9F5F1BDF9F5F1BDF9F5F1BDF9F5F1BDF9F5F1BDF9F5F0BDF5F0
    E9BDDAC3ABBDB78757800000000000000000000000000000000000000000B885
    5928B786568EB88756B1B88756B1B88756B1B88756B1B88756B1B88756B1B886
    57A9B6855763BB88551E0000000000000000000000000000000000000000}
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 741
    Height = 218
    inherited FGPanel: TPanel
      Width = 737
      Height = 214
      object MessagesView: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 737
        Height = 214
        Align = alClient
        Alignment = taRightJustify
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
        HintMode = hmTooltip
        PopupMenu = TBXPopupMenu
        TabOrder = 0
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnDblClick = MessagesViewDblClick
        OnGetText = MessagesViewGetText
        OnInitNode = MessagesViewInitNode
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Position = 0
            Text = 'Message'
            Width = 377
          end
          item
            Position = 1
            Text = 'File Name'
            Width = 200
          end
          item
            Alignment = taRightJustify
            Position = 2
            Text = 'Line'
          end
          item
            Alignment = taRightJustify
            Position = 3
            Text = 'Position'
            Width = 60
          end
          item
            MinWidth = 50
            Options = [coAllowClick, coParentBidiMode, coParentColor, coShowDropMark, coVisible, coStyleColor]
            Position = 4
          end>
      end
    end
  end
  object Panel1: TPanel [1]
    Left = 702
    Top = 2
    Width = 39
    Height = 17
    Anchors = [akTop, akRight]
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      39
      17)
    object BtnNextMsg: TButton
      Left = 20
      Top = 0
      Width = 19
      Height = 17
      Hint = 'Show next messages'
      Anchors = [akTop, akRight]
      Caption = '4'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Webdings'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = actNextMsgsExecute
    end
    object BtnPrevMsg: TButton
      Left = 0
      Top = 0
      Width = 20
      Height = 17
      Hint = 'Show previous messages'
      Anchors = [akTop, akRight]
      Caption = '3'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Webdings'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = actPreviousMsgsExecute
    end
  end
  inherited DockClient: TJvDockClient
    Left = 6
    Top = 12
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    OnPopup = TBXPopupMenuPopup
    Left = 106
    Top = 71
    object mnPreviousMessage: TSpTBXItem
      Action = actPreviousMsgs
    end
    object mnNextMessage: TSpTBXItem
      Action = actNextMsgs
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClearall: TSpTBXItem
      Action = actClearAll
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnCopy: TSpTBXItem
      Action = actCopyToClipboard
    end
  end
  object MsgsActionList: TActionList
    Images = vilImages
    Left = 25
    Top = 71
    object actClearAll: TAction
      Caption = '&Clear all'
      Hint = 'Clear all messages'
      ImageIndex = 1
      ImageName = 'Delete'
      OnExecute = ClearAllExecute
    end
    object actPreviousMsgs: TAction
      Caption = '&Previous Messages'
      Hint = 'Show previous messages'
      ImageIndex = 2
      ImageName = 'ArrowLeft'
      OnExecute = actPreviousMsgsExecute
    end
    object actNextMsgs: TAction
      Caption = '&Next Messages'
      Hint = 'Show next messages'
      ImageIndex = 3
      ImageName = 'ArrowRight'
      OnExecute = actNextMsgsExecute
    end
    object actCopyToClipboard: TAction
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy contents to Clipboard'
      ImageIndex = 0
      ImageName = 'Copy'
      OnExecute = actCopyToClipboardExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ArrowLeft'
        Name = 'ArrowLeft'
      end
      item
        CollectionIndex = 3
        CollectionName = 'ArrowRight'
        Name = 'ArrowRight'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 179
    Top = 70
  end
end
