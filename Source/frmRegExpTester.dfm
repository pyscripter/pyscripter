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
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 384
    Height = 478
    inherited FGPanel: TPanel
      Width = 380
      Height = 474
      ParentBackground = False
      object TBXDock: TSpTBXDock
        Left = 0
        Top = 0
        Width = 380
        Height = 30
        AllowDrag = False
        DoubleBuffered = True
        object RegExpTesterToolbar: TSpTBXToolbar
          Left = 0
          Top = 0
          Align = alTop
          AutoResize = False
          DockMode = dmCannotFloat
          DockPos = 1
          FullSize = True
          Images = vilImages
          TabOrder = 0
          Customizable = False
          object TiClear: TSpTBXItem
            Caption = 'Clear'
            Hint = 'Clear all fields'
            ImageIndex = 0
            ImageName = 'Delete'
            OnClick = TiClearClick
          end
          object TBXSubmenuItem2: TSpTBXSubmenuItem
            Caption = 'Options'
            ImageIndex = 4
            ImageName = 'Setup'
            object CI_DOTALL: TSpTBXItem
              Caption = 'DOTALL'
              Hint = 'Sets the DOTALL re flag'
              AutoCheck = True
              Checked = True
              HelpContext = 865
              Options = [tboShowHint]
            end
            object CI_IGNORECASE: TSpTBXItem
              Caption = 'IGNORECASE'
              Hint = 'Sets the IGNORECASE re flag'
              AutoCheck = True
              Checked = True
              HelpContext = 865
              Options = [tboShowHint]
            end
            object CI_LOCALE: TSpTBXItem
              Caption = 'LOCALE'
              Hint = 'Sets the LOCALE re flag'
              AutoCheck = True
              HelpContext = 865
              Options = [tboShowHint]
            end
            object CI_MULTILINE: TSpTBXItem
              Caption = 'MULTILINE'
              Hint = 'Sets the MULTILINE re flag'
              AutoCheck = True
              Checked = True
              HelpContext = 865
              Options = [tboShowHint]
            end
            object CI_UNICODE: TSpTBXItem
              Caption = 'UNICODE'
              Hint = 'Sets the UNICODE re flag'
              AutoCheck = True
              Checked = True
              HelpContext = 865
              Options = [tboShowHint]
            end
            object CI_VERBOSE: TSpTBXItem
              Caption = 'VERBOSE'
              Hint = 'Sets the VERBOSE re flag'
              AutoCheck = True
              Checked = True
              HelpContext = 865
              Options = [tboShowHint]
            end
            object TBXSeparatorItem2: TSpTBXSeparatorItem
            end
            object RI_Match: TSpTBXItem
              Caption = 'Match'
              Hint = 'Performs re.match'
              AutoCheck = True
              GroupIndex = 1
              HelpContext = 865
              Options = [tboShowHint]
              RadioItem = True
            end
            object RI_Search: TSpTBXItem
              Caption = 'Search'
              Hint = 'Performs re.search'
              AutoCheck = True
              Checked = True
              GroupIndex = 1
              HelpContext = 865
              Options = [tboShowHint]
              RadioItem = True
            end
            object RI_findall: TSpTBXItem
              Caption = 'Find all'
              Hint = 'Performs re.findall'
              AutoCheck = True
              GroupIndex = 1
              HelpContext = 865
              Options = [tboShowHint]
              RadioItem = True
            end
            object TBXSeparatorItem4: TSpTBXSeparatorItem
            end
            object CI_AutoExecute: TSpTBXItem
              Caption = 'Auto Execute'
              AutoCheck = True
              HelpContext = 865
            end
          end
          object TBXSeparatorItem1: TSpTBXSeparatorItem
          end
          object TIExecute: TSpTBXItem
            Caption = 'Execute'
            Hint = 'Execute search or match'
            ImageIndex = 1
            ImageName = 'Execute'
            OnClick = TIExecuteClick
          end
          object TBXSeparatorItem3: TSpTBXSeparatorItem
          end
          object tiHelp: TSpTBXItem
            Caption = 'Help'
            Hint = 'Show Python Help on the re module'
            ImageIndex = 5
            ImageName = 'Help'
            OnClick = tiHelpClick
          end
        end
      end
      object StatusBar: TSpTBXStatusBar
        Left = 0
        Top = 444
        Width = 380
        Height = 30
        Images = vilImages
        SizeGrip = False
        object lbStatusBar: TSpTBXLabelItem
          Caption = 'Not executed'
          ImageIndex = 3
          ImageName = 'Stop'
        end
      end
      object pnlBackground: TPanel
        Left = 0
        Top = 30
        Width = 380
        Height = 414
        Align = alClient
        ParentColor = True
        TabOrder = 2
        object dpGroupsView: TPanel
          Left = 1
          Top = 313
          Width = 378
          Height = 100
          Align = alBottom
          ParentColor = True
          TabOrder = 0
          object TBXLabel1: TSpTBXLabel
            Left = 1
            Top = 1
            Width = 376
            Height = 21
            Caption = 'Groups:'
            Align = alTop
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = [fsBold]
            ParentFont = False
            Wrapping = twEndEllipsis
            Underline = True
            UnderlineColor = 16746564
          end
          object GroupsView: TVirtualStringTree
            Left = 1
            Top = 22
            Width = 376
            Height = 77
            Align = alClient
            BevelEdges = []
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            Header.AutoSizeIndex = 2
            Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
            HintMode = hmTooltip
            TabOrder = 0
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
            TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
            TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
            TreeOptions.StringOptions = [toAutoAcceptEditChange]
            OnGetText = GroupsViewGetText
            Touch.InteractiveGestures = [igPan, igPressAndTap]
            Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
            Columns = <
              item
                Alignment = taRightJustify
                Margin = 1
                Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
                Position = 0
                Spacing = 1
                Text = '#'
                Width = 20
              end
              item
                Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
                Position = 1
                Text = 'Name'
                Width = 60
              end
              item
                Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
                Position = 2
                Text = 'Value'
                Width = 296
              end>
          end
        end
        object SpTBXSplitter1: TSpTBXSplitter
          Left = 1
          Top = 308
          Width = 378
          Height = 5
          Cursor = crSizeNS
          Align = alBottom
          ParentColor = False
        end
        object dpRegExpText: TPanel
          Left = 1
          Top = 1
          Width = 378
          Height = 100
          Align = alTop
          Constraints.MinHeight = 40
          ParentColor = True
          TabOrder = 1
          object SpTBXPanel1: TPanel
            Left = 1
            Top = 1
            Width = 376
            Height = 98
            Align = alClient
            ParentColor = True
            TabOrder = 0
            object TBXLabel3: TSpTBXLabel
              Left = 1
              Top = 1
              Width = 374
              Height = 21
              Caption = 'Regular Expression'
              Align = alTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Shell Dlg 2'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Underline = True
              UnderlineColor = 16746564
            end
            object RegExpText: TSynEdit
              Left = 1
              Top = 22
              Width = 374
              Height = 75
              Cursor = crDefault
              Align = alClient
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
              RightEdge = 0
              ScrollBars = ssVertical
              SelectedColor.Alpha = 0.400000005960464500
              WordWrap = True
              OnChange = RegExpTextChange
            end
          end
        end
        object SpTBXSplitter2: TSpTBXSplitter
          Left = 1
          Top = 101
          Width = 378
          Height = 5
          Cursor = crSizeNS
          Align = alTop
          ParentColor = False
        end
        object pnlMiddle: TPanel
          Left = 1
          Top = 106
          Width = 378
          Height = 202
          Align = alClient
          Constraints.MinHeight = 40
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          object dpSearchText: TPanel
            Left = 1
            Top = 1
            Width = 376
            Height = 120
            Align = alTop
            ParentColor = True
            TabOrder = 0
            object SpTBXPanel2: TPanel
              Left = 1
              Top = 1
              Width = 374
              Height = 118
              Align = alClient
              TabOrder = 0
              object TBXLabel4: TSpTBXLabel
                Left = 1
                Top = 1
                Width = 372
                Height = 21
                Caption = 'Search Text:'
                Align = alTop
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'MS Shell Dlg 2'
                Font.Style = [fsBold]
                ParentFont = False
                Wrapping = twEndEllipsis
                Underline = True
                UnderlineColor = 16746564
              end
              object SearchText: TSynEdit
                Left = 1
                Top = 22
                Width = 372
                Height = 95
                Cursor = crDefault
                Align = alClient
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
                RightEdge = 0
                ScrollBars = ssVertical
                SelectedColor.Alpha = 0.400000005960464500
                WordWrap = True
                OnChange = RegExpTextChange
              end
            end
          end
          object SpTBXSplitter3: TSpTBXSplitter
            Left = 1
            Top = 121
            Width = 376
            Height = 5
            Cursor = crSizeNS
            Align = alTop
            ParentColor = False
          end
          object dpMatchText: TPanel
            Left = 1
            Top = 126
            Width = 376
            Height = 75
            Align = alClient
            ParentColor = True
            TabOrder = 1
            object SpTBXPanel3: TPanel
              Left = 1
              Top = 1
              Width = 374
              Height = 73
              Align = alClient
              Constraints.MinHeight = 40
              ParentColor = True
              TabOrder = 0
              DesignSize = (
                374
                73)
              object lblMatch: TSpTBXLabel
                Left = 1
                Top = 1
                Width = 372
                Height = 21
                Caption = 'Match:'
                Align = alTop
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'MS Shell Dlg 2'
                Font.Style = [fsBold]
                ParentFont = False
                Wrapping = twEndEllipsis
                Underline = True
                UnderlineColor = 16746564
              end
              object SpinMatches: TSpTBXSpinEdit
                Left = 335
                Top = 0
                Width = 41
                Height = 21
                Hint = 'Show a found match'
                Anchors = [akTop, akRight]
                DoubleBuffered = True
                Enabled = False
                NumbersOnly = True
                ParentDoubleBuffered = False
                TabOrder = 2
                ExtendedAccept = True
                SpinButton.Left = 27
                SpinButton.Top = 0
                SpinButton.Width = 14
                SpinButton.Height = 21
                SpinButton.Align = alRight
                SpinButton.Enabled = False
                SpinOptions.Decimal = 0
                SpinOptions.MaxValue = 1.000000000000000000
                SpinOptions.MinValue = 1.000000000000000000
                SpinOptions.Value = 1.000000000000000000
                OnValueChanged = SpinMatchesValueChanged
              end
              object MatchText: TSynEdit
                Left = 1
                Top = 22
                Width = 372
                Height = 50
                Cursor = crDefault
                Align = alClient
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
                ScrollBars = ssVertical
                SelectedColor.Alpha = 0.400000005960464500
                WordWrap = True
              end
            end
          end
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    TopDock = False
    BottomDock = False
    Left = 16
    Top = 58
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Execute'
        Name = 'Execute'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Check'
        Name = 'Check'
      end
      item
        CollectionIndex = 111
        CollectionName = 'Stop'
        Name = 'Stop'
      end
      item
        CollectionIndex = 103
        CollectionName = 'Setup'
        Name = 'Setup'
      end
      item
        CollectionIndex = 52
        CollectionName = 'Help'
        Name = 'Help'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 40
    Top = 376
  end
end
