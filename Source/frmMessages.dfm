inherited MessagesWindow: TMessagesWindow
  Left = 259
  Top = 257
  HelpContext = 440
  ActiveControl = MessagesView
  Caption = 'Messages'
  ClientHeight = 218
  ClientWidth = 741
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
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
            Position = 0
            Text = 'Message'
            Width = 377
          end
          item
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
            Position = 1
            Text = 'File Name'
            Width = 200
          end
          item
            Alignment = taRightJustify
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
            Position = 2
            Text = 'Line'
          end
          item
            Alignment = taRightJustify
            Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
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
