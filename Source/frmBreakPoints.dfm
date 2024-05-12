inherited BreakPointsWindow: TBreakPointsWindow
  Left = 491
  Top = 381
  HelpContext = 495
  Caption = 'Breakpoints'
  ClientHeight = 244
  ClientWidth = 379
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 379
    Height = 244
    inherited FGPanel: TPanel
      Width = 375
      Height = 240
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 375
        Height = 240
        Align = alClient
        TabOrder = 0
        object BreakPointsView: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 373
          Height = 238
          Align = alClient
          Alignment = taRightJustify
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = 2
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
          HintMode = hmTooltip
          PopupMenu = TBXPopupMenu
          TabOrder = 0
          TreeOptions.AnimationOptions = [toAnimatedToggle]
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnChecked = BreakPointsViewChecked
          OnDblClick = BreakPointLVDblClick
          OnGetText = BreakPointsViewGetText
          OnInitNode = BreakPointsViewInitNode
          OnKeyDown = BreakPointsViewKeyDown
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 0
              Text = 'File Name'
              Width = 200
            end
            item
              Alignment = taRightJustify
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 1
              Text = 'Line'
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 2
              Text = 'Condition'
              Width = 123
            end>
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 12
    Top = 18
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    OnPopup = TBXPopupMenuPopup
    Left = 21
    Top = 66
    object mnSetCondition: TSpTBXItem
      Caption = 'Set &Condition...'
      OnClick = mnSetConditionClick
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClear: TSpTBXItem
      Caption = 'C&lear'
      Hint = 'Clear|Clear selected breakpoint'
      OnClick = mnClearClick
    end
    object Breakpoints1: TSpTBXItem
      Action = PyIDEMainForm.actClearAllBreakpoints
      ImageIndex = 1
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
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 7
        CollectionName = 'BreakpointsRemove'
        Name = 'BreakpointsRemove'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 16
    Top = 120
  end
end
