inherited UnitTestWindow: TUnitTestWindow
  HelpContext = 467
  Caption = 'Unit Tests'
  ClientHeight = 451
  ClientWidth = 262
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000060D00004E3C1603802A1104740000002C00000000000000000000
    0000000000000000001E250F036C3D1904830F00005B0000000E000000000000
    000048120083FF6F1EE9FF8E3FEAFFAF6AEBB47A57CD00000028000000250000
    002400000015A7714DBDFFB471EBFF9143EAFF7521EA5519009B000000050000
    0028F16E24E1F97122E2ED7A35DBEE9A5EDCFFC896E2344040B4001626EB0011
    23DF2A2B2B96FFCA9AE2EE9E64DCED7F39DBF77123DFFB7528EA020000420600
    003CFF9144ECE66A22E4EA7A36DCED985CDAFFB78DDB72B8C0F600DBFFFF00CA
    FFFF64AABCF4FFBD90DBED9C61DBEA7F3BDCE36921E2FF9448F20F0000560000
    000BB36635C1FFAE6AFDDF7936E0F1975AD9EAB790E09CA59CFD000B11FF0009
    11FF909E9AFBF2BD96DDEE9C5FD9DC7E3BE1FFA962F8C07541D3000000190000
    00000000001EAE724CBEFFC590FED49A67E7D8AE8AE7B8A798FF1A2C2AFF0F21
    1FFFB4A296FEDEB596E5D09C6AE8FFC18BFBB37A58CC0000002C000000000000
    00000000000000000010966243B0FED4AAFCB5B39CF8CDDFD8FF62F5F7FF53E9
    F5FFC5DBDBFFB8B4A0F5FFD3A2F6A07152C10000001B00000000000000000000
    000000000000000000000000000EAE7855B7E9E6D4FFB4D5DBFF94EAFBFF7EDE
    F7FFACD5DEFFEDE3CAFABD8864C7000000190000000000000000000000000000
    000C0000002D00000A560000003D00020967B7B8B8FAE5FEF2FF33C5EEFF25C0
    EEFFDEFEF7FFC8C5C0FB00060E780000044700000D5C000000310000000C0000
    002600000035000000240000093E00000B500009197CC0D0D5FD77E3F9FF6EDF
    FEFFCDD8DBFE000B167E00000647000000330000001E00000B310000001E0000
    0000000000000000000000000000000000120005318102305CB73DB7FFFF41C0
    FFFF063C6EC9000023820000000F000000000000000000000000000000000000
    0000000000000000002E0000116000052A7D0000003600174AAB00B5FFFF00A7
    FFFF00113CA90000023B0004267300000D590000002F00000000000000000000
    00000000001900000E53000000180000000900000005021135CA2478B3FF236F
    AAFF000924B700000002000000090000001600000E5400000017000000000000
    000000000430000000180000000000000000000000000000001C0000167C0000
    117B000000190000000000000000000000000000001B0000002C000000000000
    0000000000040000000100000000000000000000000000000030000000230000
    0026000000310000000000000000000000000000000100000004000000000000
    0000000000000000000000000000000000000000051400000021000000000000
    000000000022000000150000000000000000000000000000000000000000C3C1
    00008001000000000000000000000000000080010000C0030000E00700000000
    000000000000F00F0000C00300008C3100009C390000FC3F0000F99F0000}
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 262
    Height = 451
    inherited FGPanel: TPanel
      Width = 258
      Height = 447
      object ExplorerDock: TSpTBXDock
        Left = 0
        Top = 0
        Width = 258
        Height = 30
        AllowDrag = False
        DoubleBuffered = True
        object ExplorerToolbar: TSpTBXToolbar
          Left = 0
          Top = 0
          Align = alTop
          AutoResize = False
          DockMode = dmCannotFloat
          FullSize = True
          Images = vilImages
          TabOrder = 0
          Caption = 'ExplorerToolbar'
          Customizable = False
          object tbiRefresh: TSpTBXItem
            Action = actRefresh
          end
          object tbiClearAll: TSpTBXItem
            Action = actClearAll
          end
          object TBXSeparatorItem2: TSpTBXSeparatorItem
          end
          object tbiRun: TSpTBXItem
            Action = actRun
          end
          object tbiStop: TSpTBXItem
            Action = actStop
          end
          object TBXSeparatorItem1: TSpTBXSeparatorItem
          end
          object tbiSelectAll: TSpTBXItem
            Action = actSelectAll
          end
          object tbiDeselectAll: TSpTBXItem
            Action = actDeselectAll
          end
          object tbiSelectFailed: TSpTBXItem
            Action = actSelectFailed
          end
          object TBXSeparatorItem7: TSpTBXSeparatorItem
          end
          object tbiCollapseAll: TSpTBXItem
            Action = actCollapseAll
          end
          object tbiExpandAll: TSpTBXItem
            Action = actExpandAll
          end
        end
      end
      object SpTBXSplitter1: TSpTBXSplitter
        Left = 0
        Top = 269
        Width = 258
        Height = 5
        Cursor = crSizeNS
        Align = alBottom
        ParentColor = False
        MinSize = 1
      end
      object Panel1: TPanel
        Left = 0
        Top = 30
        Width = 258
        Height = 239
        Align = alClient
        TabOrder = 1
        object UnitTests: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 256
          Height = 237
          Align = alClient
          BorderStyle = bsNone
          Header.AutoSizeIndex = -1
          Header.Height = 15
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag]
          HintMode = hmHint
          Images = vilRunImages
          IncrementalSearch = isAll
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
          TreeOptions.PaintOptions = [toHideFocusRect, toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnChange = UnitTestsChange
          OnChecked = UnitTestsChecked
          OnDblClick = UnitTestsDblClick
          OnGetText = UnitTestsGetText
          OnGetImageIndex = UnitTestsGetImageIndex
          OnGetHint = UnitTestsGetHint
          OnInitChildren = UnitTestsInitChildren
          OnInitNode = UnitTestsInitNode
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <>
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 274
        Width = 258
        Height = 173
        Align = alBottom
        TabOrder = 2
        DesignSize = (
          258
          173)
        object Bevel1: TBevel
          Left = 8
          Top = 58
          Width = 242
          Height = 5
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object Label2: TLabel
          Left = 7
          Top = 62
          Width = 77
          Height = 15
          Caption = 'Error Message:'
          Color = clNone
          ParentColor = False
          Transparent = True
        end
        object ModuleName: TLabel
          Left = 7
          Top = 1
          Width = 88
          Height = 13
          Caption = 'No Module Loaded'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Transparent = True
        end
        object lbFoundTests: TLabel
          Left = 172
          Top = 1
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Found 0 tests'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Transparent = True
        end
        object lblRunTests: TLabel
          Left = 7
          Top = 19
          Width = 55
          Height = 13
          Caption = 'Run 0 tests'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Transparent = True
        end
        object lblFailures: TLabel
          Left = 7
          Top = 37
          Width = 96
          Height = 13
          Caption = 'Failures/Errors : 0/0'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = True
        end
        object SpTBXPanel1: TPanel
          Left = 1
          Top = 85
          Width = 256
          Height = 87
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          object ErrorText: TSynEdit
            Left = 1
            Top = 1
            Width = 254
            Height = 85
            Cursor = crDefault
            Align = alClient
            Constraints.MinHeight = 10
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
            SelectedColor.Alpha = 0.400000005960464500
          end
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    TopDock = False
    BottomDock = False
    Top = 48
  end
  object DialogActions: TActionList
    Images = vilImages
    Left = 152
    Top = 48
    object actRefresh: TAction
      Category = 'Commands'
      Caption = '&Refresh'
      Hint = 'Refresh tests|Extract tests from active module'
      ImageIndex = 3
      ImageName = 'Refresh'
      OnExecute = actRefreshExecute
    end
    object actRun: TAction
      Category = 'Commands'
      Caption = '&Run'
      Hint = 'Run selected tests'
      ImageIndex = 5
      ImageName = 'Run'
      OnExecute = actRunExecute
    end
    object actStop: TAction
      Category = 'Commands'
      Caption = '&Stop'
      Hint = 'Stop Testing'
      ImageIndex = 4
      ImageName = 'Abort'
      OnExecute = actStopExecute
    end
    object actSelectAll: TAction
      Category = 'TestTree'
      Caption = 'Select &All'
      Hint = 'Select all tests'
      ImageIndex = 6
      ImageName = 'TreeSelectAll'
      OnExecute = actSelectAllExecute
    end
    object actDeselectAll: TAction
      Category = 'TestTree'
      Caption = '&Deselect All'
      Hint = 'Deselect all tests'
      ImageIndex = 7
      ImageName = 'TreeDeselectAll'
      OnExecute = actDeselectAllExecute
    end
    object actSelectFailed: TAction
      Category = 'TestTree'
      Caption = 'Select Fai&led'
      Hint = 'Select all failed tests'
      ImageIndex = 8
      ImageName = 'TestsFailed'
      OnExecute = actSelectFailedExecute
    end
    object actExpandAll: TAction
      Category = 'TestTree'
      Caption = 'Ex&pand All'
      Hint = 'Expand all test nodes'
      ImageIndex = 1
      ImageName = 'Expand'
      OnExecute = actExpandAllExecute
    end
    object actCollapseAll: TAction
      Category = 'TestTree'
      Caption = '&Collapse All'
      Hint = 'Collapse all test nodes'
      ImageIndex = 2
      ImageName = 'Collapse'
      OnExecute = actCollapseAllExecute
    end
    object actClearAll: TAction
      Category = 'Commands'
      Caption = '&Clear All'
      Hint = 'Clear all tests'
      ImageIndex = 0
      ImageName = 'Delete'
      OnExecute = actClearAllExecute
    end
  end
  object vilRunImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'UnitTests\NotRun'
        Name = 'NotRun'
      end
      item
        CollectionIndex = 1
        CollectionName = 'UnitTests\Running'
        Name = 'Running'
      end
      item
        CollectionIndex = 2
        CollectionName = 'UnitTests\Success'
        Name = 'Success'
      end
      item
        CollectionIndex = 3
        CollectionName = 'UnitTests\Failure'
        Name = 'Failure'
      end
      item
        CollectionIndex = 4
        CollectionName = 'UnitTests\Error'
        Name = 'Error'
      end
      item
        CollectionIndex = 5
        CollectionName = 'UnitTests\TestsCollapsed'
        Name = 'TestsCollapsed'
      end
      item
        CollectionIndex = 6
        CollectionName = 'UnitTests\TestsExpanded'
        Name = 'TestsExpanded'
      end>
    ImageCollection = icRunImages
    PreserveItems = True
    Left = 88
    Top = 112
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
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
        CollectionIndex = 88
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Abort'
        Name = 'Abort'
      end
      item
        CollectionIndex = 93
        CollectionName = 'Run'
        Name = 'Run'
      end
      item
        CollectionIndex = 126
        CollectionName = 'TreeSelectAll'
        Name = 'TreeSelectAll'
      end
      item
        CollectionIndex = 125
        CollectionName = 'TreeDeselectAll'
        Name = 'TreeDeselectAll'
      end
      item
        CollectionIndex = 118
        CollectionName = 'TestsFailed'
        Name = 'TestsFailed'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 152
    Top = 112
  end
  object icRunImages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'UnitTests\NotRun'
        SVGText = 
          '<svg viewBox="0 0 16 16" stroke="black" >'#13#10'  <rect fill="none" x' +
          '="3.5" y="3.5" rx="1" width="9" height="9"/>'#13#10' </svg>'
      end
      item
        IconName = 'UnitTests\Running'
        SVGText = 
          '<svg viewBox="0 0 16 16" stroke="black" >'#13#10'  <rect fill="#4488FF' +
          '" x="3.5" y="3.5" rx="1" width="9" height="9"/>'#13#10' </svg>'
      end
      item
        IconName = 'UnitTests\Success'
        SVGText = 
          '<svg viewBox="0 0 16 16" stroke="black" >'#13#10'  <rect fill="#73E529' +
          '" x="3.5" y="3.5" rx="1" width="9" height="9"/>'#13#10' </svg>'
      end
      item
        IconName = 'UnitTests\Failure'
        SVGText = 
          '<svg viewBox="0 0 16 16" stroke="black" >'#13#10'  <rect fill="#E24444' +
          '" x="3.5" y="3.5" rx="1" width="9" height="9"/>'#13#10' </svg>'
      end
      item
        IconName = 'UnitTests\Error'
        SVGText = 
          '<svg viewBox="0 0 16 16" stroke="black" >'#13#10'  <rect fill="red" x=' +
          '"3.5" y="3.5" rx="1" width="9" height="9"/>'#13#10' </svg>'
      end
      item
        IconName = 'UnitTests\TestsCollapsed'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10'<g transform="matrix(1.5716713,0,0,1' +
          '.5716713,41.536863,24.655989)">'#13#10#9'<path fill="#4488FF" d="M-18.6' +
          '-0.6c-0.4,0-0.8-0.2-1.1-0.4l-4.6-4.6c-0.6-0.6-0.6-1.5,0-2.1l7.2-' +
          '7.2c0.6-0.6,1.6-0.6,2.1,0l4.6,4.6'#13#10#9#9'c0.6,0.6,0.6,1.5,0,2.1L-17.' +
          '5-1C-17.8-0.7-18.2-0.6-18.6-0.6z M-21-6.7l2.5,2.5l5.1-5.1l-2.5-2' +
          '.5L-21-6.7z"/>'#13#10#9'<path fill="#E24444" d="M-17.6,4.3c-0.3,0-0.5-0' +
          '.1-0.7-0.3l-2.8-2.8c-0.4-0.4-0.4-1,0-1.4l4.4-4.4c0.4-0.4,1-0.4,1' +
          '.4,0l2.8,2.8'#13#10#9#9'c0.2,0.2,0.3,0.4,0.3,0.7s-0.1,0.5-0.3,0.7L-16.9,' +
          '4C-17.1,4.2-17.3,4.3-17.6,4.3z M-18.9,0.6l1.3,1.3l3-3L-16-2.4L-1' +
          '8.9,0.6z"/>'#13#10#9'<path fill="#73E529" d="M-11.6,1.6c-0.3,0-0.5-0.1-' +
          '0.7-0.3l-4.4-4.4c-0.4-0.4-0.4-1,0-1.4l2.8-2.8c0.4-0.4,1-0.4,1.4,' +
          '0l4.4,4.4'#13#10#9#9'c0.4,0.4,0.4,1,0,1.4l-2.8,2.8C-11.1,1.5-11.3,1.6-11' +
          '.6,1.6z M-14.6-3.8l3,3l1.3-1.3l-3-3L-14.6-3.8z"/>'#13#10'</g>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'UnitTests\TestsExpanded'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10#9'<path d="M23.5,23.1c0,0.8-0.7,1.5-1' +
          '.5,1.5h-8.2c-0.8,0-1.5-0.7-1.5-1.5V12.7H8.8c-0.8,0-1.5-0.7-1.5-1' +
          '.5c0-0.8,0.7-1.5,1.5-1.5h12'#13#10#9#9'c0.8,0,1.5,0.7,1.5,1.5c0,0.8-0.7,' +
          '1.5-1.5,1.5h-5.5v8.9H22C22.8,21.6,23.5,22.3,23.5,23.1z"/>'#13#10#9'<pat' +
          'h fill="#73E529" d="M23.5,18.7c-0.4,0-0.9-0.2-1.2-0.5l-3.8-3.8c-' +
          '0.7-0.7-0.7-1.8,0-2.5L25,5.3c0.7-0.7,1.8-0.7,2.5,0l3.8,3.8'#13#10#9#9'c0' +
          '.7,0.7,0.7,1.8,0,2.5l-6.6,6.6C24.4,18.5,24,18.7,23.5,18.7z M22.1' +
          ',13.1l1.4,1.4l4.1-4.1L26.2,9L22.1,13.1z"/>'#13#10#9'<path fill="#4488FF' +
          '" d="M6.3,15.7c-0.4,0-0.9-0.2-1.2-0.5l-4.5-4.5c-0.7-0.7-0.7-1.8,' +
          '0-2.5l7.7-7.7c0.7-0.7,1.8-0.7,2.5,0L15.2,5'#13#10#9#9'c0.7,0.7,0.7,1.8,0' +
          ',2.5l-7.7,7.7C7.2,15.5,6.7,15.7,6.3,15.7z M4.3,9.5l2,2l5.2-5.2l-' +
          '2-2L4.3,9.5z"/>'#13#10#9'<path fill="#E24444" d="M23,32c-0.4,0-0.9-0.2-' +
          '1.2-0.5l-3.8-3.8c-0.7-0.7-0.7-1.8,0-2.5l6.6-6.6c0.7-0.7,1.8-0.7,' +
          '2.5,0l3.8,3.8'#13#10#9#9'c0.7,0.7,0.7,1.8,0,2.5l-6.6,6.6C23.9,31.8,23.5,' +
          '32,23,32z M21.7,26.4l1.4,1.4l4.1-4.1l-1.4-1.4L21.7,26.4z"/>'#13#10'</s' +
          'vg>'#13#10
      end>
    ApplyFixedColorToRootOnly = True
    Left = 24
    Top = 112
  end
end
