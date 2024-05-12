inherited RegExpTesterWindow: TRegExpTesterWindow
  HelpContext = 865
  Caption = 'Regular Expressions'
  ClientHeight = 478
  ClientWidth = 384
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
            Height = 26
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
            Top = 27
            Width = 376
            Height = 72
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
              Height = 26
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
              Top = 27
              Width = 374
              Height = 70
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
                Height = 26
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
                Top = 27
                Width = 372
                Height = 90
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
                Height = 26
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
                Top = 27
                Width = 372
                Height = 45
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
