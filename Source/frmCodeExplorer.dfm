inherited CodeExplorerWindow: TCodeExplorerWindow
  Left = 237
  Top = 237
  HelpContext = 430
  Caption = 'Code Explorer'
  TextHeight = 15
  inherited BGPanel: TPanel
    inherited FGPanel: TPanel
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 223
        Height = 394
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ExplorerTree: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 223
          Height = 394
          Align = alClient
          BorderStyle = bsNone
          Colors.UnfocusedColor = clHighlightText
          Colors.UnfocusedSelectionColor = clGray
          Colors.UnfocusedSelectionBorderColor = clGray
          Header.AutoSizeIndex = -1
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
          HintMode = hmHint
          Images = vilCodeImages
          IncrementalSearch = isAll
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toWheelPanning]
          TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toRightClickSelect]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnChange = ExplorerTreeChange
          OnCollapsed = ExplorerTreeCollapsed
          OnContextPopup = ExplorerTreeContextPopup
          OnDblClick = ExplorerTreeDblClick
          OnExpanded = ExplorerTreeExpanded
          OnGetCellText = ExplorerTreeGetCellText
          OnGetImageIndex = ExplorerTreeGetImageIndex
          OnGetHint = ExplorerTreeGetHint
          OnInitChildren = ExplorerTreeInitChildren
          OnInitNode = ExplorerTreeInitNode
          OnKeyPress = ExplorerTreeKeyPress
          OnScroll = ExplorerTreeScroll
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coStyleColor]
              Position = 0
              Width = 223
            end>
        end
      end
    end
  end
  object CEPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 24
    Top = 61
    object mnExpandAll: TSpTBXItem
      Caption = '&Expand All'
      Hint = 'Expand all nodes'
      HelpContext = 430
      ImageIndex = 0
      ImageName = 'Expand'
      OnClick = mnExpandAllClick
    end
    object nCollapseAll: TSpTBXItem
      Caption = '&Collapse All'
      Hint = 'Collapse all nodes'
      HelpContext = 430
      ImageIndex = 1
      ImageName = 'Collapse'
      OnClick = nCollapseAllClick
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnAlphaSort: TSpTBXItem
      Caption = 'Alpha Sort'
      Hint = 'Sort Alphabetically'
      AutoCheck = True
      HelpContext = 430
      OnClick = mnAlphaSortClick
    end
    object mnFollowEditor: TSpTBXItem
      Caption = 'Follow Editor'
      Hint = 'Move selection to match the editor position'
      AutoCheck = True
      Checked = True
      HelpContext = 430
      OnClick = mnFollowEditorClick
    end
    object mnShowSelection: TSpTBXItem
      Caption = 'Show Selection'
      Hint = 'Show selected node in editor'
      AutoCheck = True
      Checked = True
      HelpContext = 430
    end
  end
  object CENodePopUpMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 28
    Top = 120
    object mnFindDefinition: TSpTBXItem
      Caption = 'Find Definition'
      Hint = 'Find Definition'
      HelpContext = 430
      OnClick = mnFindDefinitionClick
    end
    object mnFindReferences: TSpTBXItem
      Caption = 'Find References'
      Hint = 'Find References'
      HelpContext = 430
      OnClick = mnFindReferencesClick
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnHighlight: TSpTBXItem
      Caption = 'Highlight'
      Hint = 'Highlight References'
      HelpContext = 430
      ImageIndex = 2
      ImageName = 'Highlight'
      OnClick = mnHighlightClick
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
    Top = 176
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 31
        CollectionName = 'Expand'
        Name = 'Expand'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Collapse'
        Name = 'Collapse'
      end
      item
        CollectionIndex = 53
        CollectionName = 'Highlight'
        Name = 'Highlight'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 24
    Top = 232
  end
end
