inherited CallStackWindow: TCallStackWindow
  Left = 569
  Top = 397
  HelpContext = 470
  Caption = 'Call Stack'
  ClientHeight = 168
  ClientWidth = 604
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 604
    Height = 168
    inherited FGPanel: TPanel
      Width = 600
      Height = 164
      object Panel1: TPanel
        Left = 190
        Top = 0
        Width = 410
        Height = 164
        Align = alClient
        TabOrder = 0
        object CallStackView: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 408
          Height = 162
          Align = alClient
          Alignment = taRightJustify
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelKind = bkFlat
          BorderStyle = bsNone
          Colors.UnfocusedColor = clHighlightText
          Colors.UnfocusedSelectionColor = clGray
          Colors.UnfocusedSelectionBorderColor = clGray
          Header.AutoSizeIndex = 1
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoVisible]
          HintMode = hmTooltip
          StateImages = vilImages
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
          OnAddToSelection = CallStackViewAddToSelection
          OnDblClick = CallStackViewDblClick
          OnFreeNode = CallStackViewFreeNode
          OnGetCellText = CallStackViewGetCellText
          OnGetImageIndex = CallStackViewGetImageIndex
          OnInitNode = CallStackViewInitNode
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 0
              Text = 'Function Name'
              Width = 100
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 1
              Text = 'File Name'
              Width = 258
            end
            item
              Alignment = taRightJustify
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 2
              Text = 'Line'
            end>
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 164
        Align = alLeft
        TabOrder = 1
        object ThreadView: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 183
          Height = 162
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BevelKind = bkFlat
          BorderStyle = bsNone
          Colors.UnfocusedColor = clHighlightText
          Colors.UnfocusedSelectionColor = clGray
          Colors.UnfocusedSelectionBorderColor = clGray
          Header.AutoSizeIndex = 0
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoFullRepaintOnResize]
          Images = vilImages
          NodeDataSize = 0
          StateImages = vilImages
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
          OnAddToSelection = ThreadViewAddToSelection
          OnGetCellText = ThreadViewGetCellText
          OnGetImageIndex = ThreadViewGetImageIndex
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 0
              Text = 'Threads'
              Width = 183
            end>
        end
      end
      object Splitter1: TSpTBXSplitter
        Left = 185
        Top = 0
        Height = 164
        Cursor = crSizeWE
        ParentColor = False
        MinSize = 3
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 559
    Top = 33
  end
  object actlCallStack: TActionList
    OnUpdate = actlCallStackUpdate
    Left = 492
    Top = 33
    object actPreviousFrame: TAction
      Category = 'Run'
      Caption = 'Previous Frame'
      Enabled = False
      HelpContext = 470
      HelpType = htContext
      Hint = 'Select previous (older) frame'
      ShortCut = 122
      OnExecute = actPreviousFrameExecute
    end
    object actNextFrame: TAction
      Category = 'Run'
      Caption = 'Next Frame'
      Enabled = False
      HelpContext = 470
      HelpType = htContext
      Hint = 'Select next (newer) frame'
      ShortCut = 8314
      OnExecute = actNextFrameExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 78
        CollectionName = 'Pin'
        Name = 'Pin'
      end
      item
        CollectionIndex = 131
        CollectionName = 'ThreadRunning'
        Name = 'ThreadRunning'
      end
      item
        CollectionIndex = 130
        CollectionName = 'ThreadPaused'
        Name = 'ThreadPaused'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 423
    Top = 32
  end
end
