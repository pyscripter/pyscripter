inherited VariablesWindow: TVariablesWindow
  Left = 237
  Top = 237
  HelpContext = 480
  Caption = 'Variables'
  ClientHeight = 247
  ClientWidth = 579
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 579
    Height = 247
    inherited FGPanel: TPanel
      Width = 575
      Height = 243
      object DocPanel: TSpTBXPageScroller
        Left = 200
        Top = 0
        Width = 375
        Height = 243
        Align = alRight
        Color = clNone
        ParentColor = False
        TabOrder = 0
        object synInfo: TSynEdit
          Left = 0
          Top = 0
          Width = 375
          Height = 89
          Cursor = crDefault
          Align = alTop
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 0
          UseCodeFolding = False
          BorderStyle = bsNone
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Visible = False
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 13
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkTrackChanges
            end
            item
              Kind = gbkMargin
              Width = 3
            end>
          HideSelection = True
          ReadOnly = True
          RightEdge = 0
          ScrollBars = ssNone
          SelectedColor.Alpha = 0.400000005960464500
          WordWrap = True
        end
      end
      object SpTBXSplitter: TSpTBXSplitter
        Left = 195
        Top = 0
        Height = 243
        Cursor = crSizeWE
        Align = alRight
        ParentColor = False
        MinSize = 3
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 195
        Height = 243
        Align = alClient
        TabOrder = 2
        object VariablesTree: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 193
          Height = 241
          Align = alClient
          Alignment = taRightJustify
          Anchors = [akLeft, akTop, akBottom]
          BorderStyle = bsNone
          Header.AutoSizeIndex = 2
          Header.Images = vilCodeImages
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoShowImages, hoVisible]
          Header.PopupMenu = VTHeaderPopupMenu
          HintMode = hmTooltip
          Images = vilCodeImages
          IncrementalSearch = isVisibleOnly
          TabOrder = 0
          TreeOptions.AnimationOptions = [toAnimatedToggle]
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnAddToSelection = VariablesTreeAddToSelection
          OnFreeNode = VariablesTreeFreeNode
          OnGetCellText = VariablesTreeGetCellText
          OnPaintText = VariablesTreePaintText
          OnGetImageIndex = VariablesTreeGetImageIndex
          OnInitChildren = VariablesTreeInitChildren
          OnInitNode = VariablesTreeInitNode
          OnNodeClick = VariablesTreeNodeClick
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 0
              Text = 'Name'
              Width = 160
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 1
              Text = 'Type'
              Width = 100
            end
            item
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coEditable, coStyleColor]
              Position = 2
              Text = 'Value'
              Width = 10
            end
            item
              ImageIndex = 10
              Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible, coAllowFocus, coStyleColor]
              Position = 3
              Width = 20
            end>
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 24
    Top = 26
  end
  object VTHeaderPopupMenu: TVTHeaderPopupMenu
    Left = 20
    Top = 75
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
      end
      item
        CollectionIndex = 10
        CollectionName = 'Inspect'
        Name = 'Inspect'
      end>
    ImageCollection = ResourcesDataModule.icCodeImages
    PreserveItems = True
    Left = 22
    Top = 123
  end
end
