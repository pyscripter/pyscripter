inherited WatchesWindow: TWatchesWindow
  Left = 331
  Top = 325
  HelpContext = 490
  Caption = 'Watches'
  ClientHeight = 229
  ClientWidth = 760
  TextHeight = 15
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
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 0
              Text = 'Watches'
              Width = 200
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 1
              Text = 'Type'
              Width = 120
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
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
      ImageName = 'Plus'
      OnClick = mnAddWatchClick
    end
    object TBXItem1: TSpTBXItem
      Action = PyIDEMainForm.actAddWatchAtCursor
      ImageIndex = 4
      ImageName = 'Watch'
    end
    object mnRemoveWatch: TSpTBXItem
      Caption = '&Remove Watch'
      ImageIndex = 3
      ImageName = 'Minus'
      OnClick = mnRemoveWatchClick
    end
    object mnEditWatch: TSpTBXItem
      Caption = '&Edit Watch'
      ImageIndex = 5
      ImageName = 'Edit'
      OnClick = mnEditWatchClick
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClearall: TSpTBXItem
      Caption = '&Clear all'
      Hint = 'Clear all watches'
      ImageIndex = 1
      ImageName = 'Delete'
      OnClick = mnClearAllClick
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnCopyToClipboard: TSpTBXItem
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 0
      ImageName = 'Copy'
      OnClick = mnCopyToClipboardClick
    end
  end
  object vilCodeImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 8
        CollectionName = 'CodeImages\Python'
        Name = 'Python'
      end
      item
        CollectionIndex = 9
        CollectionName = 'CodeImages\Variable'
        Name = 'Variable'
      end
      item
        CollectionIndex = 1
        CollectionName = 'CodeImages\Field'
        Name = 'Field'
      end
      item
        CollectionIndex = 2
        CollectionName = 'CodeImages\Function'
        Name = 'Function'
      end
      item
        CollectionIndex = 5
        CollectionName = 'CodeImages\Method'
        Name = 'Method'
      end
      item
        CollectionIndex = 0
        CollectionName = 'CodeImages\Class'
        Name = 'Class'
      end
      item
        CollectionIndex = 7
        CollectionName = 'CodeImages\Namespace'
        Name = 'Namespace'
      end
      item
        CollectionIndex = 4
        CollectionName = 'CodeImages\List'
        Name = 'List'
      end
      item
        CollectionIndex = 6
        CollectionName = 'CodeImages\Module'
        Name = 'Module'
      end
      item
        CollectionIndex = 3
        CollectionName = 'CodeImages\Keyword'
        Name = 'Keyword'
      end>
    ImageCollection = ResourcesDataModule.icCodeImages
    PreserveItems = True
    Left = 24
    Top = 136
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
        CollectionIndex = 68
        CollectionName = 'Plus'
        Name = 'Plus'
      end
      item
        CollectionIndex = 62
        CollectionName = 'Minus'
        Name = 'Minus'
      end
      item
        CollectionIndex = 135
        CollectionName = 'Watch'
        Name = 'Watch'
      end
      item
        CollectionIndex = 24
        CollectionName = 'Edit'
        Name = 'Edit'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 88
    Top = 136
  end
end
