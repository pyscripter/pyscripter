inherited CodeExplorerWindow: TCodeExplorerWindow
  Left = 237
  Top = 237
  HelpContext = 430
  Caption = 'Code Explorer'
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000995EC423773AA0CD995EC4230000000000000000000000001111
    11640F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F
    0F852F203996783CA1FF9E62C7FF69328BE82F2039960F0F0F85111111642727
    277BEBEBEBFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
    E7FF7A3EA3FFAE72D7FFD397FCFFAA6ED3FF8C5CADFFDFD7E5FF2727277B3131
    3176EAEAEAFFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FF7C7C7CFF7C7C
    7CFF7C7C7CFF8E52B7FFD397FCFFD397FCFFDDA1FFFF783CA1FF313131763737
    3773EDEDEDFFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FF7C7C7CFFE6E6
    E6FFE6E6E6FFDBD3E1FF9D61C6FFDDA1FFFF773BA0FFE1D9E7FF373737733C3C
    3C71F0F0F0FFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFF7C7C7CFFEBEB
    EBFFE6E0D1FFB69138FFDCCFCCFF773BA0FFDFD7E5FFF0F0F0FF3C3C3C714343
    436EF3F3F3FFCFE0E4FF0D8AA8FFCFE0E4FFEFEFEFFFEFEFEFFF7C7C7CFFEAE3
    D4FFAB7D0EFFD1A334FFB69137FFEAE3D4FFEFEFEFFFF3F3F3FF4343436E4949
    496CD6E7EBFF0D8CABFF21A0BFFF2995AFFFD3E5E9FFF4F4F4FF7C7C7CFFAD7F
    10FFE1B344FFFFD869FFDDAF40FFB8933AFFEEE8D8FFF7F7F7FF4949496C385D
    657E0F8EADFF28A7C6FF1291B0FF1998B7FF2995B0FF7C7C7CFF7C7C7CFF7C7C
    7CFFC19324FFFFD869FFFFD869FFFFE273FFAB7D0EFFFAFAFAFF4C4C4C6A0884
    A1ED29A8C7FF26A5C4FF1291B0FF1291B0FF209FBEFF2996B0FFDAEBF0FFFCFC
    FCFFF5EFDFFFD0A233FFFFE273FFAA7C0DFFF5EFDFFFFDFDFDFF505050683F62
    6A7C0F8EADFF38B7D6FF2DACCBFF21A0BFFF21A0BFFF28A7C6FF2A96B1FFDDEE
    F2FFFFFFFFFFF8F1E2FFAA7C0DFFF8F1E2FFFFFFFFFFFFFFFFFF54545467D692
    3DE1CEAC6EFF1291B0FF44C3E2FF3BBAD9FF30AFCEFF30AFCEFF45C4E3FF0785
    A3FFDB9F53FFD89C50FFD69A4EFFD3974BFFD19549FFCF9347FFB06B14E0D391
    3FCDF4C375FDDCC78BFF1493B2FF50CFEEFF49C8E7FF48C7E6FF0785A3FFD2B3
    73FFF3BC60FFF4BE50FFF7C23EFFF9C82BFFFBCD19FFF2C020FDD3913FCDC28A
    455CD3953FCDD9983EE0B89250E41796B5FF5AD9F8FF0B829EFEB89250E4D998
    3EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D3953FCDC28A455C0000
    0000000000000000000000000000078AA723108FB1F3078AA723000000000000
    000000000000000000000000000000000000000000000000000000000000}
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
