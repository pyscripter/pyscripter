inherited fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 521
  Top = 154
  HelpContext = 620
  Anchors = [akRight, akBottom]
  Caption = 'Editor Options'
  ClientHeight = 421
  ClientWidth = 459
  Font.Name = 'MS Shell Dlg 2'
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  ExplicitWidth = 465
  ExplicitHeight = 447
  DesignSize = (
    459
    421)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 444
    Height = 349
    ActivePage = Options
    Anchors = [akLeft, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object Display: TTabSheet
      Caption = 'Display'
      DesignSize = (
        436
        321)
      object gbRightEdge: TSpTBXGroupBox
        Left = 210
        Top = 102
        Width = 223
        Height = 75
        Caption = 'Right Edge'
        ThemeType = thtWindows
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        object cbRightEdgeColor: TColorBox
          Left = 88
          Top = 44
          Width = 132
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clDefault
          Selected = clInactiveBorder
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
          DropDownCount = 20
          ItemHeight = 16
          TabOrder = 1
        end
        object Label3: TSpTBXLabel
          Left = 9
          Top = 50
          Width = 54
          Height = 13
          Caption = 'Edge color:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object Label10: TSpTBXLabel
          Left = 9
          Top = 20
          Width = 66
          Height = 13
          Caption = 'Edge Column:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object eRightEdge: TSpTBXEdit
          Left = 89
          Top = 17
          Width = 51
          Height = 21
          TabOrder = 0
          Text = '0'
          ThemeType = thtWindows
        end
      end
      object gbGutter: TSpTBXGroupBox
        Left = 3
        Top = 183
        Width = 430
        Height = 124
        Caption = 'Gutter'
        ThemeType = thtWindows
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        object pnlGutterFontDisplay: TSpTBXPanel
          Left = 190
          Top = 40
          Width = 183
          Height = 27
          ThemeType = thtWindows
          TabOrder = 6
          object lblGutterFont: TSpTBXLabel
            Left = 2
            Top = 2
            Width = 179
            Height = 23
            Caption = 'Terminal 8pt'
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Terminal'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            Alignment = taCenter
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
            ExplicitHeight = 12
          end
        end
        object cbGutterColor: TColorBox
          Left = 295
          Top = 71
          Width = 132
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clWindow
          Selected = clWhite
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
          DropDownCount = 20
          ItemHeight = 16
          TabOrder = 7
        end
        object btnGutterFont: TSpTBXButton
          Left = 328
          Top = 11
          Width = 99
          Height = 25
          Caption = 'Font'
          TabOrder = 5
          OnClick = btnGutterFontClick
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object ckGutterAutosize: TSpTBXCheckBox
          Left = 23
          Top = 38
          Width = 59
          Height = 15
          Caption = 'Autosize'
          ParentColor = True
          TabOrder = 1
          ThemeType = thtWindows
        end
        object ckGutterShowLineNumbers: TSpTBXCheckBox
          Left = 23
          Top = 59
          Width = 107
          Height = 15
          Caption = 'Show line numbers'
          ParentColor = True
          TabOrder = 2
          ThemeType = thtWindows
        end
        object ckGutterShowLeaderZeros: TSpTBXCheckBox
          Left = 23
          Top = 100
          Width = 110
          Height = 15
          Caption = 'Show leading zeros'
          ParentColor = True
          TabOrder = 3
          ThemeType = thtWindows
        end
        object ckGutterVisible: TSpTBXCheckBox
          Left = 23
          Top = 18
          Width = 47
          Height = 15
          Caption = 'Visible'
          ParentColor = True
          TabOrder = 0
          Checked = True
          State = cbChecked
          ThemeType = thtWindows
        end
        object cbGutterFont: TSpTBXCheckBox
          Left = 189
          Top = 18
          Width = 95
          Height = 15
          Caption = 'Use Gutter Font'
          ParentColor = True
          TabOrder = 4
          OnClick = cbGutterFontClick
          ThemeType = thtWindows
        end
        object ckGutterStartAtZero: TSpTBXCheckBox
          Left = 23
          Top = 79
          Width = 79
          Height = 15
          Caption = 'Start at zero'
          ParentColor = True
          TabOrder = 8
          ThemeType = thtWindows
        end
        object ckGutterGradient: TSpTBXCheckBox
          Left = 191
          Top = 100
          Width = 93
          Height = 15
          Hint = 'Gutter gradient visible'
          Caption = 'Gutter Gradient'
          ParentColor = True
          TabOrder = 9
          ThemeType = thtWindows
        end
        object Label1: TSpTBXLabel
          Left = 189
          Top = 73
          Width = 61
          Height = 13
          Caption = 'Gutter color:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
      end
      object gbBookmarks: TSpTBXGroupBox
        Left = 210
        Top = 46
        Width = 223
        Height = 55
        Caption = 'Bookmarks'
        ThemeType = thtWindows
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        object ckBookmarkKeys: TSpTBXCheckBox
          Left = 23
          Top = 16
          Width = 89
          Height = 15
          Caption = 'Bookmark keys'
          ParentColor = True
          TabOrder = 0
          ThemeType = thtWindows
        end
        object ckBookmarkVisible: TSpTBXCheckBox
          Left = 23
          Top = 35
          Width = 101
          Height = 15
          Caption = 'Bookmarks visible'
          ParentColor = True
          TabOrder = 1
          ThemeType = thtWindows
        end
      end
      object gbEditorFont: TSpTBXGroupBox
        Left = 3
        Top = 1
        Width = 199
        Height = 84
        Caption = 'Editor Font'
        ThemeType = thtWindows
        TabOrder = 4
        object Panel3: TSpTBXPanel
          Left = 8
          Top = 17
          Width = 172
          Height = 30
          ThemeType = thtWindows
          TabOrder = 1
          object labFont: TSpTBXLabel
            Left = 2
            Top = 2
            Width = 168
            Height = 26
            Caption = 'Courier New 10pt'
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            Alignment = taCenter
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
            ExplicitHeight = 16
          end
        end
        object btnFont: TSpTBXButton
          Left = 53
          Top = 49
          Width = 92
          Height = 25
          Caption = 'Font'
          TabOrder = 0
          OnClick = btnFontClick
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
      end
      object gbLineSpacing: TSpTBXGroupBox
        Left = 3
        Top = 91
        Width = 198
        Height = 88
        Caption = 'Line spacing / Tab spacing'
        ThemeType = thtWindows
        TabOrder = 2
        object Label8: TSpTBXLabel
          Left = 23
          Top = 27
          Width = 57
          Height = 13
          Caption = 'Extra Lines:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object Label9: TSpTBXLabel
          Left = 23
          Top = 56
          Width = 53
          Height = 13
          Caption = 'Tab Width:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object eLineSpacing: TSpTBXEdit
          Left = 116
          Top = 23
          Width = 48
          Height = 21
          TabOrder = 0
          Text = '0'
          ThemeType = thtWindows
        end
        object eTabWidth: TSpTBXEdit
          Left = 116
          Top = 53
          Width = 48
          Height = 21
          TabOrder = 1
          Text = '8'
          ThemeType = thtWindows
        end
      end
      object GroupBox2: TSpTBXGroupBox
        Left = 210
        Top = 3
        Width = 223
        Height = 43
        Caption = 'Active Line Color'
        ThemeType = thtWindows
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        object cbActiveLineColor: TColorBox
          Left = 23
          Top = 15
          Width = 136
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clNone
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
      end
    end
    object Options: TTabSheet
      Caption = 'Options'
      DesignSize = (
        436
        321)
      object gbOptions: TSpTBXGroupBox
        Left = 8
        Top = 0
        Width = 420
        Height = 247
        Caption = 'Options'
        ThemeType = thtWindows
        Anchors = [akLeft, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object ckAutoIndent: TSpTBXCheckBox
          Left = 23
          Top = 15
          Width = 74
          Height = 15
          Hint = 
            'Will indent the caret on new lines with the same amount of leadi' +
            'ng white space as the preceding line'
          Caption = 'Auto indent'
          ParentColor = True
          TabOrder = 0
          ThemeType = thtWindows
        end
        object ckDragAndDropEditing: TSpTBXCheckBox
          Left = 23
          Top = 34
          Width = 122
          Height = 15
          Hint = 
            'Allows you to select a block of text and drag it within the docu' +
            'ment to another location'
          Caption = 'Drag and drop editing'
          ParentColor = True
          TabOrder = 1
          ThemeType = thtWindows
        end
        object ckWordWrap: TSpTBXCheckBox
          Left = 23
          Top = 53
          Width = 71
          Height = 15
          Hint = 'Allows the editor accept OLE file drops'
          Caption = 'Word wrap'
          ParentColor = True
          TabOrder = 2
          ThemeType = thtWindows
        end
        object ckHalfPageScroll: TSpTBXCheckBox
          Left = 215
          Top = 15
          Width = 91
          Height = 15
          Hint = 
            'When scrolling with page-up and page-down commands, only scroll ' +
            'a half page at a time'
          Caption = 'Half page scroll'
          ParentColor = True
          TabOrder = 11
          ThemeType = thtWindows
        end
        object ckThemeSelection: TSpTBXCheckBox
          Left = 215
          Top = 224
          Width = 95
          Height = 15
          Hint = 'Themes the selection color'
          Caption = 'Theme selection'
          ParentColor = True
          TabOrder = 20
          ThemeType = thtWindows
        end
        object ckScrollByOneLess: TSpTBXCheckBox
          Left = 215
          Top = 34
          Width = 100
          Height = 15
          Hint = 'Forces scrolling to be one less'
          Caption = 'Scroll by one less'
          ParentColor = True
          TabOrder = 12
          ThemeType = thtWindows
        end
        object ckScrollPastEOF: TSpTBXCheckBox
          Left = 215
          Top = 53
          Width = 118
          Height = 15
          Hint = 'Allows the cursor to go past the end of file marker'
          Caption = 'Scroll past end of file'
          ParentColor = True
          TabOrder = 13
          ThemeType = thtWindows
        end
        object ckScrollPastEOL: TSpTBXCheckBox
          Left = 215
          Top = 72
          Width = 120
          Height = 15
          Hint = 
            'Allows the cursor to go past the last character into the white s' +
            'pace at the end of a line'
          Caption = 'Scroll past end of line'
          ParentColor = True
          TabOrder = 14
          ThemeType = thtWindows
        end
        object ckShowScrollHint: TSpTBXCheckBox
          Left = 215
          Top = 91
          Width = 92
          Height = 15
          Hint = 
            'Shows a hint of the visible line numbers when scrolling vertical' +
            'ly'
          Caption = 'Show scroll hint'
          ParentColor = True
          TabOrder = 15
          ThemeType = thtWindows
        end
        object ckSmartTabs: TSpTBXCheckBox
          Left = 23
          Top = 129
          Width = 70
          Height = 15
          Hint = 
            'When tabbing, the cursor will go to the next non-white space cha' +
            'racter of the previous line'
          Caption = 'Smart tabs'
          ParentColor = True
          TabOrder = 6
          ThemeType = thtWindows
        end
        object ckTabsToSpaces: TSpTBXCheckBox
          Left = 23
          Top = 167
          Width = 90
          Height = 15
          Hint = 'Converts a tab character to the number of spaces in Tab Width'
          Caption = 'Tabs to spaces'
          ParentColor = True
          TabOrder = 17
          ThemeType = thtWindows
        end
        object ckTrimTrailingSpaces: TSpTBXCheckBox
          Left = 215
          Top = 148
          Width = 109
          Height = 15
          Hint = 'Spaces at the end of lines will be trimmed and not saved'
          Caption = 'Trim trailing spaces'
          ParentColor = True
          TabOrder = 18
          ThemeType = thtWindows
        end
        object ckTabIndent: TSpTBXCheckBox
          Left = 23
          Top = 110
          Width = 71
          Height = 15
          Hint = 'Tab indents and Shft-Tab unindents'
          Caption = 'Tab Indent'
          ParentColor = True
          TabOrder = 5
          ThemeType = thtWindows
        end
        object ckAltSetsColumnMode: TSpTBXCheckBox
          Left = 23
          Top = 72
          Width = 119
          Height = 15
          Hint = 
            'Holding down the Alt Key will put the selection mode into column' +
            'ar format'
          Caption = 'Alt sets column mode'
          ParentColor = True
          TabOrder = 3
          ThemeType = thtWindows
        end
        object ckKeepCaretX: TSpTBXCheckBox
          Left = 23
          Top = 91
          Width = 122
          Height = 15
          Hint = 
            'When moving through lines the X position will always stay the sa' +
            'me'
          Caption = 'Maintain caret column'
          ParentColor = True
          TabOrder = 4
          ThemeType = thtWindows
        end
        object ckScrollHintFollows: TSpTBXCheckBox
          Left = 215
          Top = 110
          Width = 134
          Height = 15
          Hint = 'The scroll hint follows the mouse when scrolling vertically'
          Caption = 'Scroll hint follows mouse'
          ParentColor = True
          TabOrder = 16
          ThemeType = thtWindows
        end
        object ckGroupUndo: TSpTBXCheckBox
          Left = 215
          Top = 167
          Width = 74
          Height = 15
          Hint = 
            'When undoing/redoing actions, handle all continous changes of th' +
            'e same kind in one call instead undoing/redoing each command sep' +
            'arately'
          Caption = 'Group undo'
          ParentColor = True
          TabOrder = 19
          ThemeType = thtWindows
        end
        object ckSmartTabDelete: TSpTBXCheckBox
          Left = 23
          Top = 148
          Width = 98
          Height = 15
          Hint = 'similar to Smart Tabs, but when you delete characters'
          Caption = 'Smart tab delete'
          ParentColor = True
          TabOrder = 7
          ThemeType = thtWindows
        end
        object ckRightMouseMoves: TSpTBXCheckBox
          Left = 23
          Top = 186
          Width = 144
          Height = 15
          Hint = 
            'When clicking with the right mouse for a popup menu, move the cu' +
            'rsor to that location'
          Caption = 'Right mouse moves cursor'
          ParentColor = True
          TabOrder = 8
          ThemeType = thtWindows
        end
        object ckEnhanceHomeKey: TSpTBXCheckBox
          Left = 23
          Top = 205
          Width = 110
          Height = 15
          Hint = 'enhances home key positioning, similar to visual studio'
          Caption = 'Enhance Home Key'
          ParentColor = True
          TabOrder = 9
          ThemeType = thtWindows
        end
        object ckHideShowScrollbars: TSpTBXCheckBox
          Left = 215
          Top = 129
          Width = 153
          Height = 15
          Hint = 
            'if enabled, then the scrollbars will only show when necessary.  ' +
            'If you have ScrollPastEOL, then it the horizontal bar will alway' +
            's be there (it uses MaxLength instead)'
          Caption = 'Hide scrollbars as necessary'
          ParentColor = True
          TabOrder = 10
          ThemeType = thtWindows
        end
        object ckDisableScrollArrows: TSpTBXCheckBox
          Left = 215
          Top = 186
          Width = 115
          Height = 15
          Hint = 
            'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
            'hat direction any more'
          Caption = 'Disable scroll arrows'
          ParentColor = True
          TabOrder = 21
          ThemeType = thtWindows
        end
        object ckShowSpecialChars: TSpTBXCheckBox
          Left = 215
          Top = 205
          Width = 108
          Height = 15
          Hint = 'Shows linebreaks, spaces and tabs using special symbols'
          Caption = 'Show special chars'
          ParentColor = True
          TabOrder = 22
          ThemeType = thtWindows
        end
        object ckEnhanceEndKey: TSpTBXCheckBox
          Left = 23
          Top = 224
          Width = 101
          Height = 15
          Hint = 'Enhances end key similar to JDeveloper'
          Caption = 'Enhance End Key'
          ParentColor = True
          TabOrder = 23
          ThemeType = thtWindows
        end
      end
      object gbCaret: TSpTBXGroupBox
        Left = 8
        Top = 249
        Width = 420
        Height = 69
        Caption = 'Caret'
        ThemeType = thtWindows
        Anchors = [akTop, akRight]
        TabOrder = 1
        object Label2: TSpTBXLabel
          Left = 60
          Top = 19
          Width = 61
          Height = 13
          Caption = 'Insert caret:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object Label4: TSpTBXLabel
          Left = 60
          Top = 43
          Width = 80
          Height = 13
          Caption = 'Overwrite caret:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object cInsertCaret: TSpTBXComboBox
          Left = 183
          Top = 14
          Width = 176
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
          ThemeType = thtWindows
        end
        object cOverwriteCaret: TSpTBXComboBox
          Left = 183
          Top = 38
          Width = 176
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
          ThemeType = thtWindows
        end
      end
    end
    object Keystrokes: TTabSheet
      Caption = 'Keystrokes'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbKeyStrokes: TSpTBXGroupBox
        Left = 10
        Top = 191
        Width = 415
        Height = 119
        Caption = 'Keystroke Options'
        ThemeType = thtWindows
        TabOrder = 4
        object Label5: TSpTBXLabel
          Left = 50
          Top = 28
          Width = 51
          Height = 13
          Caption = 'Command:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object Label6: TSpTBXLabel
          Left = 50
          Top = 91
          Width = 52
          Height = 13
          Caption = 'Keystroke:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object Label7: TSpTBXLabel
          Left = 50
          Top = 59
          Width = 52
          Height = 13
          Caption = 'Keystroke:'
          ParentColor = True
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object cKeyCommand: TSpTBXComboBox
          Left = 154
          Top = 23
          Width = 186
          Height = 21
          ItemHeight = 0
          TabOrder = 0
          OnExit = cKeyCommandExit
          OnKeyPress = cKeyCommandKeyPress
          OnKeyUp = cKeyCommandKeyUp
          ThemeType = thtWindows
        end
      end
      object pnlCommands: TSpTBXPanel
        Left = 8
        Top = 9
        Width = 417
        Height = 136
        ThemeType = thtWindows
        TabOrder = 0
        object KeyList: TTntListView
          Left = 2
          Top = 2
          Width = 413
          Height = 132
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Command'
              Width = 167
            end
            item
              Caption = 'Keystroke'
              Width = 142
            end>
          FlatScrollBars = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnChanging = KeyListChanging
          OnColumnClick = KeyListColumnClick
        end
      end
      object btnAddKey: TSpTBXButton
        Left = 166
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddKeyClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object btnRemKey: TSpTBXButton
        Left = 246
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Remove'
        TabOrder = 3
        OnClick = btnRemKeyClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object btnUpdateKey: TSpTBXButton
        Left = 86
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Update'
        TabOrder = 1
        OnClick = btnUpdateKeyClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
    end
    object Color: TTabSheet
      Caption = 'Syntax Colors'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        436
        321)
      object cbElementForeground: TColorBox
        Left = 2
        Top = 183
        Width = 151
        Height = 22
        DefaultColorColor = clNone
        NoneColorColor = clNone
        Selected = clNone
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 1
        OnChange = cbElementForegroundChange
      end
      object cbElementBackground: TColorBox
        Left = 2
        Top = 227
        Width = 151
        Height = 22
        DefaultColorColor = clNone
        NoneColorColor = clNone
        Selected = clNone
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 2
        OnChange = cbElementBackgroundChange
      end
      object GroupBox1: TSpTBXGroupBox
        Left = 3
        Top = 251
        Width = 150
        Height = 62
        Caption = ' Text attributes '
        ThemeType = thtWindows
        TabOrder = 3
        object cbxElementBold: TSpTBXCheckBox
          Left = 9
          Top = 17
          Width = 38
          Height = 15
          Caption = '&Bold'
          Enabled = False
          ParentColor = True
          TabOrder = 0
          OnClick = cbxElementBoldClick
          ThemeType = thtWindows
        end
        object cbxElementItalic: TSpTBXCheckBox
          Left = 9
          Top = 40
          Width = 41
          Height = 15
          Caption = '&Italic'
          Enabled = False
          ParentColor = True
          TabOrder = 1
          OnClick = cbxElementBoldClick
          ThemeType = thtWindows
        end
        object cbxElementUnderline: TSpTBXCheckBox
          Left = 67
          Top = 17
          Width = 63
          Height = 15
          Caption = '&Underline'
          Enabled = False
          ParentColor = True
          TabOrder = 2
          OnClick = cbxElementBoldClick
          ThemeType = thtWindows
        end
        object cbxElementStrikeout: TSpTBXCheckBox
          Left = 67
          Top = 40
          Width = 66
          Height = 15
          Caption = '&Strike Out'
          Enabled = False
          ParentColor = True
          TabOrder = 3
          OnClick = cbxElementBoldClick
          ThemeType = thtWindows
        end
      end
      object SynEdit1: TSynEdit
        Left = 165
        Top = 58
        Width = 268
        Height = 254
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 4
        OnClick = SynEdit1Click
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Gutter.Width = 0
        RightEdge = 0
      end
      object Label11: TSpTBXLabel
        Left = 5
        Top = 44
        Width = 42
        Height = 13
        Caption = '&Element:'
        ParentColor = True
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object Label12: TSpTBXLabel
        Left = 5
        Top = 171
        Width = 88
        Height = 13
        Caption = '&Foreground Color:'
        ParentColor = True
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object Label13: TSpTBXLabel
        Left = 5
        Top = 211
        Width = 88
        Height = 13
        Caption = 'B&ackground Color:'
        ParentColor = True
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object Label14: TSpTBXLabel
        Left = 164
        Top = 45
        Width = 62
        Height = 13
        Caption = 'Code Sample'
        ParentColor = True
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object Label15: TSpTBXLabel
        Left = 5
        Top = 3
        Width = 115
        Height = 13
        Caption = 'Editor Syntax Language'
        ParentColor = True
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object cbHighlighters: TSpTBXComboBox
        Left = 3
        Top = 18
        Width = 427
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 5
        OnChange = cbHighlightersChange
        ThemeType = thtWindows
      end
      object lbElements: TSpTBXListBox
        Left = 2
        Top = 58
        Width = 151
        Height = 107
        Style = lbStandard
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbElementsClick
        ThemeType = thtWindows
      end
    end
  end
  object btnOk: TSpTBXButton
    Left = 216
    Top = 389
    Width = 73
    Height = 25
    Caption = '&OK'
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = btnOkClick
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
  end
  object btnCancel: TSpTBXButton
    Left = 297
    Top = 389
    Width = 73
    Height = 25
    Caption = '&Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object btnHelp: TSpTBXButton
    Left = 376
    Top = 388
    Width = 75
    Height = 25
    Caption = '&Help'
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnClick = btnHelpClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object cbApplyToAll: TSpTBXCheckBox
    Left = 19
    Top = 366
    Width = 107
    Height = 15
    Caption = 'Apply to all editors'
    ParentColor = True
    TabOrder = 3
    Checked = True
    State = cbChecked
    ThemeType = thtWindows
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 386
    Top = 5
  end
end
