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
  PixelsPerInch = 96
  TextHeight = 13
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
        Height = 26
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
        Top = 26
        Width = 258
        Height = 243
        Align = alClient
        TabOrder = 1
        object UnitTests: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 256
          Height = 241
          Align = alClient
          BorderStyle = bsNone
          Header.AutoSizeIndex = -1
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag]
          HintMode = hmHint
          Images = vilRunImages
          IncrementalSearch = isAll
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
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
          Width = 73
          Height = 13
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
          object ErrorText: TRichEdit
            Left = 1
            Top = 1
            Width = 254
            Height = 85
            Align = alClient
            BorderStyle = bsNone
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = []
            Constraints.MinHeight = 10
            ParentFont = False
            PlainText = True
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            Zoom = 100
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
      ImageName = 'Item40'
      OnExecute = actRefreshExecute
    end
    object actRun: TAction
      Category = 'Commands'
      Caption = '&Run'
      Hint = 'Run selected tests'
      ImageIndex = 5
      ImageName = 'Item52'
      OnExecute = actRunExecute
    end
    object actStop: TAction
      Category = 'Commands'
      Caption = '&Stop'
      Hint = 'Stop Testing'
      ImageIndex = 4
      ImageName = 'Item41'
      OnExecute = actStopExecute
    end
    object actSelectAll: TAction
      Category = 'TestTree'
      Caption = 'Select &All'
      Hint = 'Select all tests'
      ImageIndex = 6
      ImageName = 'Item105'
      OnExecute = actSelectAllExecute
    end
    object actDeselectAll: TAction
      Category = 'TestTree'
      Caption = '&Deselect All'
      Hint = 'Deselect all tests'
      ImageIndex = 7
      ImageName = 'Item106'
      OnExecute = actDeselectAllExecute
    end
    object actSelectFailed: TAction
      Category = 'TestTree'
      Caption = 'Select Fai&led'
      Hint = 'Select all failed tests'
      ImageIndex = 8
      ImageName = 'Item107'
      OnExecute = actSelectFailedExecute
    end
    object actExpandAll: TAction
      Category = 'TestTree'
      Caption = 'Ex&pand All'
      Hint = 'Expand all test nodes'
      ImageIndex = 1
      ImageName = 'Item29'
      OnExecute = actExpandAllExecute
    end
    object actCollapseAll: TAction
      Category = 'TestTree'
      Caption = '&Collapse All'
      Hint = 'Collapse all test nodes'
      ImageIndex = 2
      ImageName = 'Item30'
      OnExecute = actCollapseAllExecute
    end
    object actClearAll: TAction
      Category = 'Commands'
      Caption = '&Clear All'
      Hint = 'Clear all tests'
      ImageIndex = 0
      ImageName = 'Item15'
      OnExecute = actClearAllExecute
    end
  end
  object vilRunImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'UnitTests\NotRun'
        Disabled = False
        Name = 'NotRun'
      end
      item
        CollectionIndex = 1
        CollectionName = 'UnitTests\Running'
        Disabled = False
        Name = 'Running'
      end
      item
        CollectionIndex = 2
        CollectionName = 'UnitTests\Success'
        Disabled = False
        Name = 'Success'
      end
      item
        CollectionIndex = 3
        CollectionName = 'UnitTests\Failure'
        Disabled = False
        Name = 'Failure'
      end
      item
        CollectionIndex = 4
        CollectionName = 'UnitTests\Error'
        Disabled = False
        Name = 'Error'
      end
      item
        CollectionIndex = 5
        CollectionName = 'UnitTests\TestsCollapsed'
        Disabled = False
        Name = 'TestsCollapsed'
      end
      item
        CollectionIndex = 6
        CollectionName = 'UnitTests\TestsExpanded'
        Disabled = False
        Name = 'TestsExpanded'
      end>
    ImageCollection = icRunImages
    PreserveItems = True
    Left = 88
    Top = 112
  end
  object vilImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 14
        CollectionName = 'Item15'
        Disabled = False
        Name = 'Item15'
      end
      item
        CollectionIndex = 28
        CollectionName = 'Item29'
        Disabled = False
        Name = 'Item29'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Item30'
        Disabled = False
        Name = 'Item30'
      end
      item
        CollectionIndex = 39
        CollectionName = 'Item40'
        Disabled = False
        Name = 'Item40'
      end
      item
        CollectionIndex = 40
        CollectionName = 'Item41'
        Disabled = False
        Name = 'Item41'
      end
      item
        CollectionIndex = 51
        CollectionName = 'Item52'
        Disabled = False
        Name = 'Item52'
      end
      item
        CollectionIndex = 104
        CollectionName = 'Item105'
        Disabled = False
        Name = 'Item105'
      end
      item
        CollectionIndex = 105
        CollectionName = 'Item106'
        Disabled = False
        Name = 'Item106'
      end
      item
        CollectionIndex = 106
        CollectionName = 'Item107'
        Disabled = False
        Name = 'Item107'
      end>
    ImageCollection = CommandsDataModule.icImages
    Left = 152
    Top = 112
  end
  object icRunImages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'UnitTests\NotRun'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" xmln' +
          's:xlink="http://www.w3.org/1999/xlink"><clipPath id="a"><path cl' +
          'ip-rule="evenodd" d="m0 0v16h16v-16zm4 4v8h8v-8z"/></clipPath><r' +
          'ect clip-path="url(#a)" height="10" rx="1" width="10" x="3" y="3' +
          '"/></svg>'
      end
      item
        IconName = 'UnitTests\Running'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" xmln' +
          's:xlink="http://www.w3.org/1999/xlink"><clipPath id="a"><path cl' +
          'ip-rule="evenodd" d="m0 0v16h16v-16zm4 4v8h8v-8z"/></clipPath><r' +
          'ect clip-path="url(#a)" height="10" rx="1" width="10" x="3" y="3' +
          '"/><path d="m4 4h8v8h-8z" fill="#48f"/></svg>'
      end
      item
        IconName = 'UnitTests\Success'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" xmln' +
          's:xlink="http://www.w3.org/1999/xlink"><clipPath id="a"><path cl' +
          'ip-rule="evenodd" d="m0 0v16h16v-16zm4 4v8h8v-8z"/></clipPath><r' +
          'ect clip-path="url(#a)" height="10" rx="1" width="10" x="3" y="3' +
          '"/><path d="m4 4h8v8h-8z" fill="#73e529"/></svg>'
      end
      item
        IconName = 'UnitTests\Failure'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" xmln' +
          's:xlink="http://www.w3.org/1999/xlink"><clipPath id="a"><path cl' +
          'ip-rule="evenodd" d="m0 0v16h16v-16zm4 4v8h8v-8z"/></clipPath><r' +
          'ect clip-path="url(#a)" height="10" rx="1" width="10" x="3" y="3' +
          '"/><path d="m4 4h8v8h-8z" fill="#e24444"/></svg>'
      end
      item
        IconName = 'UnitTests\Error'
        SVGText = 
          '<svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" xmln' +
          's:xlink="http://www.w3.org/1999/xlink"><clipPath id="a"><path cl' +
          'ip-rule="evenodd" d="m0 0v16h16v-16zm4 4v8h8v-8z"/></clipPath><r' +
          'ect clip-path="url(#a)" height="10" rx="1" width="10" x="3" y="3' +
          '"/><path d="m4 4h8v8h-8z" fill="#f00"/></svg>'
      end
      item
        IconName = 'UnitTests\TestsCollapsed'
        SVGText = 
          '<svg enable-background="new 0 0 32 32" viewBox="0 0 32 32" xmlns' +
          '="http://www.w3.org/2000/svg"><g transform="matrix(1.5716713 0 0' +
          ' 1.5716713 41.536863 24.655989)"><path d="m-18.6-.6c-.4 0-.8-.2-' +
          '1.1-.4l-4.6-4.6c-.6-.6-.6-1.5 0-2.1l7.2-7.2c.6-.6 1.6-.6 2.1 0l4' +
          '.6 4.6c.6.6.6 1.5 0 2.1l-7.1 7.2c-.3.3-.7.4-1.1.4zm-2.4-6.1 2.5 ' +
          '2.5 5.1-5.1-2.5-2.5z" fill="#48f"/><path d="m-17.6 4.3c-.3 0-.5-' +
          '.1-.7-.3l-2.8-2.8c-.4-.4-.4-1 0-1.4l4.4-4.4c.4-.4 1-.4 1.4 0l2.8' +
          ' 2.8c.2.2.3.4.3.7s-.1.5-.3.7l-4.4 4.4c-.2.2-.4.3-.7.3zm-1.3-3.7 ' +
          '1.3 1.3 3-3-1.4-1.3z" fill="#e24444"/><path d="m-11.6 1.6c-.3 0-' +
          '.5-.1-.7-.3l-4.4-4.4c-.4-.4-.4-1 0-1.4l2.8-2.8c.4-.4 1-.4 1.4 0l' +
          '4.4 4.4c.4.4.4 1 0 1.4l-2.8 2.8c-.2.2-.4.3-.7.3zm-3-5.4 3 3 1.3-' +
          '1.3-3-3z" fill="#73e529"/></g></svg>'
      end
      item
        IconName = 'UnitTests\TestsExpanded'
        SVGText = 
          '<svg viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg"><pat' +
          'h d="m23.5 23.1c0 .8-.7 1.5-1.5 1.5h-8.2c-.8 0-1.5-.7-1.5-1.5v-1' +
          '0.4h-3.5c-.8 0-1.5-.7-1.5-1.5s.7-1.5 1.5-1.5h12c.8 0 1.5.7 1.5 1' +
          '.5s-.7 1.5-1.5 1.5h-5.5v8.9h6.7c.8 0 1.5.7 1.5 1.5z"/><path d="m' +
          '23.5 18.7c-.4 0-.9-.2-1.2-.5l-3.8-3.8c-.7-.7-.7-1.8 0-2.5l6.5-6.' +
          '6c.7-.7 1.8-.7 2.5 0l3.8 3.8c.7.7.7 1.8 0 2.5l-6.6 6.6c-.3.3-.7.' +
          '5-1.2.5zm-1.4-5.6 1.4 1.4 4.1-4.1-1.4-1.4z" fill="#73e529"/><pat' +
          'h d="m6.3 15.7c-.4 0-.9-.2-1.2-.5l-4.5-4.5c-.7-.7-.7-1.8 0-2.5l7' +
          '.7-7.7c.7-.7 1.8-.7 2.5 0l4.4 4.5c.7.7.7 1.8 0 2.5l-7.7 7.7c-.3.' +
          '3-.8.5-1.2.5zm-2-6.2 2 2 5.2-5.2-2-2z" fill="#48f"/><path d="m23' +
          ' 32c-.4 0-.9-.2-1.2-.5l-3.8-3.8c-.7-.7-.7-1.8 0-2.5l6.6-6.6c.7-.' +
          '7 1.8-.7 2.5 0l3.8 3.8c.7.7.7 1.8 0 2.5l-6.6 6.6c-.4.3-.8.5-1.3.' +
          '5zm-1.3-5.6 1.4 1.4 4.1-4.1-1.4-1.4z" fill="#e24444"/></svg>'
      end>
    Left = 24
    Top = 112
  end
end
