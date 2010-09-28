inherited RegExpTesterWindow: TRegExpTesterWindow
  HelpContext = 865
  Caption = 'Regular Expressions'
  ClientHeight = 478
  ClientWidth = 384
  Icon.Data = {
    0000010001001010000000000000680500001600000028000000100000002000
    0000010008000000000040010000000000000000000000010000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C000C0DC
    C000F0CAA600F0F0F00099FFFF0099D49900FFD49900FFCCFF009999FF003022
    2200110000002200000044000000550000007700000088000000AA000000DD00
    0000EE00000000110000002200000044000000550000007700000088000000AA
    000000DD000000EE000000001100000022000000440000005500000077000000
    90000000AA000000DD000000EE00330000006600000099000000CC0000000033
    00003333000066330000A1330000CC330000FF33000000660000336600006666
    000099660000CC660000FF66000000990000339900006699000099990000CC99
    0000FF99000000CC000033CC000066CC000099CC0000CCCC0000FFCC000033FF
    000066FF000099FF0000CCFF000000003300330033006600330099003300CC00
    3300FF003300003333003B3333006633330099333300CC333300FF3333000066
    3300336E33006666330099663300CC663300FF66330000993300339933006699
    330099993300CC993300FF99330000CC330033CC330066CC330099CC3300CCCC
    3300FFCC330000FF330033FF330066FF330099FF3300CCFF3300FFFF33000000
    6600330066006600660099006600CC006600FF00660000336600333366006633
    660099336600CC336600FF33660000666600336666006666660099666600CC66
    6600FF66660000996600339966006699660099996600CC996600FF99660000CC
    660033CC660066CC660099CC6600CCCC6600FFCC660000FF660033FF660066FF
    660099FF6600CCFF6600FFFF660000009900330099006600990099009900CC00
    9900FF00990000339900333399006633990099339900CC339900FF3399000066
    A100336699006666990099669900CC669900FF66990000999900339999006699
    990099999900CC999900FF99990000CC990033CC990066CC990099CC9900CCCC
    9900FFCC990000FF990033FF990066FF990099FF9900CCFF9900FFFF99000000
    CC003300CC006600CC009900CC00CC00CC00FF08D4000033CC003333CC006633
    CC009933CC00CC33CC00FF33CC000066CC003366CC006666CC009966CC00CC66
    CC00FF66CC000099CC003399CC006699CC009999CC00CC99CC00FF99CC0000CC
    CC0033CCCC0066CCCC0099CCCC00CCCCCC00FFCCCC0000FFCC0033FFCC0066FF
    CC0099FFCC00CCFFCC00FFFFCC003300FF006600FF009900FF00CC00FF000033
    FF003333FF006633FF009933FF00CC33FF00FF33FF000066FF003366FF006666
    FF009966FF00CC66FF00FF66FF000099FF00DDDDDD00CC99FF0066CCFF000000
    8800FF00CC0099330000336633000066990033333300F0FBFF00A4A0A0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000000007E7EF8F8F8F8537E0700000000000C
    78EDD50A0AFF0AD4A3537E070000077FD52701F8F8F8F8A928019B7E07007807
    7E01010AED08A30178F8019AA9005A7FF6F0017E7E780101F8F87F537E0077FF
    7EF001ABCFAB0101010101F07E00F8A9CF0101CE09ABA301A353F071CD00F8D4
    A9F0F09C9CAAA378F001A27D0000F708D4A3AACEAA7F5B547EA97E00000000CD
    A9FFCDA9F8A2A9A9A97D54070000000000F8F8F8A27E7E84F7077D7E07000000
    00000000000000CDF87E53007ED300000000000000000000F77EA853007E0000
    000000000000000000F77E7E1023000000000000000000000000F77D7E11FFFF
    0000E00F00008003000000010000000100000001000000010000000100000003
    00000007000080030000E0010000FF800000FFC00000FFE00000FFF00000}
  ExplicitWidth = 400
  ExplicitHeight = 512
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 380
    Height = 474
    ExplicitWidth = 380
    ExplicitHeight = 474
    object TBXDock: TSpTBXDock
      Left = 0
      Top = 0
      Width = 380
      Height = 26
      AllowDrag = False
      object RegExpTesterToolbar: TSpTBXToolbar
        Left = 0
        Top = 0
        Align = alTop
        AutoResize = False
        DockMode = dmCannotFloat
        FullSize = True
        Images = CommandsDataModule.Images
        TabOrder = 0
        Customizable = False
        object TiClear: TSpTBXItem
          Caption = 'Clear'
          Hint = 'Clear all fields'
          ImageIndex = 14
          OnClick = TiClearClick
        end
        object TBXSubmenuItem2: TSpTBXSubmenuItem
          Caption = 'Options'
          ImageIndex = 22
          object CI_DOTALL: TSpTBXItem
            Caption = 'DOTALL'
            Hint = 'Sets the DOTALL re flag'
            AutoCheck = True
            Checked = True
            Options = [tboShowHint]
          end
          object CI_IGNORECASE: TSpTBXItem
            Caption = 'IGNORECASE'
            Hint = 'Sets the IGNORECASE re flag'
            AutoCheck = True
            Checked = True
            Options = [tboShowHint]
          end
          object CI_LOCALE: TSpTBXItem
            Caption = 'LOCALE'
            Hint = 'Sets the LOCALE re flag'
            AutoCheck = True
            Checked = True
            Options = [tboShowHint]
          end
          object CI_MULTILINE: TSpTBXItem
            Caption = 'MULTILINE'
            Hint = 'Sets the MULTILINE re flag'
            AutoCheck = True
            Checked = True
            Options = [tboShowHint]
          end
          object CI_UNICODE: TSpTBXItem
            Caption = 'UNICODE'
            Hint = 'Sets the UNICODE re flag'
            AutoCheck = True
            Checked = True
            Options = [tboShowHint]
          end
          object CI_VERBOSE: TSpTBXItem
            Caption = 'VERBOSE'
            Hint = 'Sets the VERBOSE re flag'
            AutoCheck = True
            Checked = True
            Options = [tboShowHint]
          end
          object TBXSeparatorItem2: TSpTBXSeparatorItem
          end
          object RI_Match: TSpTBXItem
            Caption = 'Match'
            Hint = 'Performs re.match'
            AutoCheck = True
            GroupIndex = 1
            Options = [tboShowHint]
            RadioItem = True
          end
          object RI_Search: TSpTBXItem
            Caption = 'Search'
            Hint = 'Performs re.search'
            AutoCheck = True
            Checked = True
            GroupIndex = 1
            Options = [tboShowHint]
            RadioItem = True
          end
          object TBXSeparatorItem4: TSpTBXSeparatorItem
          end
          object CI_AutoExecute: TSpTBXItem
            Caption = 'Auto Execute'
            AutoCheck = True
            Checked = True
          end
        end
        object TBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object TIExecute: TSpTBXItem
          Caption = 'Execute'
          Hint = 'Execute search or match'
          ImageIndex = 19
          OnClick = TIExecuteClick
        end
        object TBXSeparatorItem3: TSpTBXSeparatorItem
        end
        object tiHelp: TSpTBXItem
          Caption = 'Help'
          Hint = 'Show Python Help on the re module'
          ImageIndex = 33
          OnClick = tiHelpClick
        end
      end
    end
    object StatusBar: TSpTBXStatusBar
      Left = 0
      Top = 448
      Width = 380
      Height = 26
      Images = CommandsDataModule.Images
      SizeGrip = False
      object lbStatusBar: TSpTBXLabelItem
        Caption = 'Not executed'
        ImageIndex = 21
      end
    end
    object pnlBackground: TSpTBXPanel
      Left = 0
      Top = 26
      Width = 380
      Height = 422
      Align = alClient
      TabOrder = 2
      Borders = False
      TBXStyleBackground = True
      object dpGroupsView: TSpTBXPanel
        Left = 0
        Top = 284
        Width = 380
        Height = 138
        Align = alBottom
        TabOrder = 0
        object TBXLabel1: TSpTBXLabel
          Left = 2
          Top = 2
          Width = 376
          Height = 20
          Caption = 'Groups:'
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Shell Dlg 2'
          Font.Style = [fsBold]
          ParentFont = False
          Wrapping = twEndEllipsis
          Underline = True
          UnderlineColor = clBlue
        end
        object GroupsView: TVirtualStringTree
          Left = 2
          Top = 22
          Width = 376
          Height = 114
          Align = alClient
          BevelEdges = []
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = 2
          Header.DefaultHeight = 17
          Header.Options = [hoAutoResize, hoColumnResize, hoOwnerDraw, hoVisible]
          Header.ParentFont = True
          HintMode = hmTooltip
          TabOrder = 0
          TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnGetText = GroupsViewGetText
          Columns = <
            item
              Alignment = taRightJustify
              Margin = 1
              Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
              Position = 0
              Spacing = 1
              Width = 20
              WideText = '#'
            end
            item
              Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
              Position = 1
              Width = 60
              WideText = 'Name'
            end
            item
              Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
              Position = 2
              Width = 296
              WideText = 'Value'
            end>
        end
      end
      object SpTBXSplitter1: TSpTBXSplitter
        Left = 0
        Top = 279
        Width = 380
        Height = 5
        Cursor = crSizeNS
        Align = alBottom
      end
      object dpRegExpText: TSpTBXPanel
        Left = 0
        Top = 0
        Width = 380
        Height = 82
        Align = alTop
        TabOrder = 1
        object TBXLabel3: TSpTBXLabel
          Left = 2
          Top = 2
          Width = 376
          Height = 20
          Caption = 'Regular Expression'
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Shell Dlg 2'
          Font.Style = [fsBold]
          ParentFont = False
          Wrapping = twEndEllipsis
          Underline = True
          UnderlineColor = clBlue
        end
        object SpTBXPanel1: TSpTBXPanel
          Left = 2
          Top = 22
          Width = 376
          Height = 58
          Caption = 'SpTBXPanel1'
          Align = alClient
          TabOrder = 0
          HotTrack = True
          object RegExpText: TRichEdit
            Left = 2
            Top = 2
            Width = 372
            Height = 54
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            Font.Charset = GREEK_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = []
            ParentFont = False
            PlainText = True
            ScrollBars = ssVertical
            TabOrder = 0
            OnChange = RegExpTextChange
          end
        end
      end
      object SpTBXSplitter2: TSpTBXSplitter
        Left = 0
        Top = 82
        Width = 380
        Height = 5
        Cursor = crSizeNS
        Align = alTop
      end
      object pnlMiddle: TSpTBXPanel
        Left = 0
        Top = 87
        Width = 380
        Height = 192
        Align = alClient
        TabOrder = 2
        Borders = False
        TBXStyleBackground = True
        object dpSearchText: TSpTBXPanel
          Left = 0
          Top = 0
          Width = 380
          Height = 95
          Align = alTop
          TabOrder = 0
          object TBXLabel4: TSpTBXLabel
            Left = 2
            Top = 2
            Width = 376
            Height = 20
            Caption = 'Search Text:'
            Align = alTop
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = [fsBold]
            ParentFont = False
            Wrapping = twEndEllipsis
            Underline = True
            UnderlineColor = clBlue
          end
          object SpTBXPanel2: TSpTBXPanel
            Left = 2
            Top = 22
            Width = 376
            Height = 71
            Caption = 'SpTBXPanel2'
            Align = alClient
            TabOrder = 0
            HotTrack = True
            object SearchText: TRichEdit
              Left = 2
              Top = 2
              Width = 372
              Height = 67
              Align = alClient
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Font.Charset = GREEK_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Shell Dlg 2'
              Font.Style = []
              ParentFont = False
              PlainText = True
              ScrollBars = ssVertical
              TabOrder = 0
              OnChange = RegExpTextChange
            end
          end
        end
        object SpTBXSplitter3: TSpTBXSplitter
          Left = 0
          Top = 95
          Width = 380
          Height = 5
          Cursor = crSizeNS
          Align = alTop
        end
        object dpMatchText: TSpTBXPanel
          Left = 0
          Top = 100
          Width = 380
          Height = 92
          Align = alClient
          TabOrder = 1
          object TBXLabel2: TSpTBXLabel
            Left = 2
            Top = 2
            Width = 376
            Height = 20
            Caption = 'Match:'
            Align = alTop
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = [fsBold]
            ParentFont = False
            Wrapping = twEndEllipsis
            Underline = True
            UnderlineColor = clBlue
          end
          object SpTBXPanel3: TSpTBXPanel
            Left = 2
            Top = 22
            Width = 376
            Height = 68
            Caption = 'SpTBXPanel3'
            Align = alClient
            TabOrder = 0
            HotTrack = True
            object MatchText: TRichEdit
              Left = 2
              Top = 2
              Width = 372
              Height = 64
              Align = alClient
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Font.Charset = GREEK_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Shell Dlg 2'
              Font.Style = []
              ParentFont = False
              PlainText = True
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    TopDock = False
    BottomDock = False
  end
end
