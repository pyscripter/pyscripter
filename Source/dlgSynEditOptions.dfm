inherited fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 521
  Top = 154
  HelpContext = 620
  Caption = 'Editor Options'
  ClientHeight = 421
  ClientWidth = 591
  ShowHint = True
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 591
    Height = 421
    Align = alClient
    TabOrder = 0
    DesignSize = (
      591
      421)
    object TabControl: TSpTBXTabControl
      Left = 1
      Top = 1
      Width = 589
      Height = 368
      Align = alTop
      ActiveTabIndex = 0
      HiddenItems = <>
      object SpTBXTabItem1: TSpTBXTabItem
        Caption = 'Display'
        Checked = True
      end
      object SpTBXTabItem2: TSpTBXTabItem
        Caption = 'Options'
      end
      object SpTBXTabItem3: TSpTBXTabItem
        Caption = 'Keystrokes'
      end
      object SpTBXTabItem5: TSpTBXTabItem
        Caption = 'Color Theme'
      end
      object SpTBXTabItem4: TSpTBXTabItem
        Caption = 'Syntax Colors'
      end
      object KeyStrokes: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 589
        Height = 343
        Caption = 'Keystrokes'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem3'
        object gbKeyStrokes: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 217
          Width = 570
          Height = 119
          Caption = 'Keystroke Options'
          TabOrder = 3
          object Label5: TLabel
            Left = 50
            Top = 28
            Width = 60
            Height = 15
            Caption = 'Command:'
          end
          object Label6: TLabel
            Left = 50
            Top = 91
            Width = 63
            Height = 15
            Caption = 'Keystroke 2:'
          end
          object Label7: TLabel
            Left = 50
            Top = 59
            Width = 63
            Height = 15
            Caption = 'Keystroke 1:'
          end
          object cKeyCommand: TComboBox
            Left = 185
            Top = 23
            Width = 268
            Height = 23
            Style = csDropDownList
            Sorted = True
            TabOrder = 0
            OnKeyUp = cKeyCommandKeyUp
          end
        end
        object btnAddKey: TButton
          Left = 245
          Top = 171
          Width = 102
          Height = 25
          Caption = '&Add'
          TabOrder = 1
          OnClick = btnAddKeyClick
        end
        object btnRemKey: TButton
          Left = 368
          Top = 171
          Width = 102
          Height = 25
          Caption = '&Remove'
          TabOrder = 2
          OnClick = btnRemKeyClick
        end
        object btnUpdateKey: TButton
          Left = 122
          Top = 171
          Width = 102
          Height = 25
          Caption = '&Update'
          TabOrder = 0
          OnClick = btnUpdateKeyClick
        end
        object Panel2: TPanel
          Left = 2
          Top = 0
          Width = 583
          Height = 158
          Align = alTop
          TabOrder = 4
          object KeyList: TListView
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 575
            Height = 150
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Command'
                Width = 200
              end
              item
                Caption = 'Keystroke'
                Width = 142
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            SortType = stText
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object ColorThemes: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 589
        Height = 343
        Caption = 'Color Theme'
        ImageIndex = -1
        DesignSize = (
          589
          343)
        TabItem = 'SpTBXTabItem5'
        object SpTBXLabel1: TLabel
          Left = 183
          Top = 12
          Width = 70
          Height = 15
          Caption = 'Code Sample'
        end
        object SpTBXLabel2: TLabel
          Left = 3
          Top = 12
          Width = 92
          Height = 15
          Caption = 'Available Themes'
        end
        object SynThemeSample: TSynEdit
          Left = 183
          Top = 41
          Width = 404
          Height = 276
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 0
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Visible = False
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 15
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkMargin
              Width = 2
            end>
          RightEdge = 0
          SelectedColor.Alpha = 0.400000005960464500
        end
        object lbColorThemes: TListBox
          Left = 3
          Top = 41
          Width = 166
          Height = 240
          ItemHeight = 15
          TabOrder = 2
          OnClick = lbColorThemesClick
          OnDblClick = btnApplyThemeClick
        end
        object btnApplyTheme: TButton
          Left = 15
          Top = 290
          Width = 130
          Height = 25
          Caption = 'Apply Theme'
          TabOrder = 1
          OnClick = btnApplyThemeClick
        end
      end
      object Color: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 589
        Height = 343
        Caption = 'Syntax Colors'
        ImageIndex = -1
        DesignSize = (
          589
          343)
        TabItem = 'SpTBXTabItem4'
        object Label11: TLabel
          Left = 15
          Top = 44
          Width = 46
          Height = 15
          Caption = '&Element:'
        end
        object Label12: TLabel
          Left = 15
          Top = 164
          Width = 97
          Height = 15
          Caption = '&Foreground Color:'
        end
        object Label13: TLabel
          Left = 15
          Top = 208
          Width = 99
          Height = 15
          Caption = 'B&ackground Color:'
        end
        object Label14: TLabel
          Left = 223
          Top = 45
          Width = 70
          Height = 15
          Caption = 'Code Sample'
        end
        object Label15: TLabel
          Left = 15
          Top = 2
          Width = 124
          Height = 15
          Caption = 'Editor Syntax Language'
        end
        object GroupBox1: TGroupBox
          Left = 15
          Top = 256
          Width = 191
          Height = 69
          Caption = ' Text attributes '
          TabOrder = 3
          DesignSize = (
            191
            69)
          object cbxElementBold: TCheckBox
            Left = 5
            Top = 17
            Width = 84
            Height = 21
            Caption = '&Bold'
            Enabled = False
            TabOrder = 0
            OnClick = cbxElementBoldClick
          end
          object cbxElementItalic: TCheckBox
            Left = 3
            Top = 40
            Width = 86
            Height = 21
            Caption = '&Italic'
            Enabled = False
            TabOrder = 1
            OnClick = cbxElementBoldClick
          end
          object cbxElementUnderline: TCheckBox
            Left = 95
            Top = 13
            Width = 93
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '&Underline'
            Enabled = False
            TabOrder = 2
            OnClick = cbxElementBoldClick
          end
          object cbxElementStrikeout: TCheckBox
            Left = 95
            Top = 40
            Width = 93
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '&Strike Out'
            Enabled = False
            TabOrder = 3
            OnClick = cbxElementBoldClick
          end
        end
        object SynSyntaxSample: TSynEdit
          Left = 223
          Top = 64
          Width = 352
          Height = 257
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 4
          OnClick = SynSyntaxSampleClick
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Visible = False
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 15
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkMargin
              Width = 2
            end>
          RightEdge = 0
          SelectedColor.Alpha = 0.400000005960464500
        end
        object cbHighlighters: TComboBox
          Left = 15
          Top = 23
          Width = 560
          Height = 23
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          OnChange = cbHighlightersChange
        end
        object lbElements: TSpTBXListBox
          Left = 15
          Top = 63
          Width = 188
          Height = 97
          Style = lbStandard
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbElementsClick
        end
        object cbElementForeground: TSpTBXColorEdit
          Left = 15
          Top = 181
          Width = 191
          Height = 23
          TabOrder = 1
          SelectedColor = clBlack
          SelectedFormat = cttHTML
          OnSelectedColorChanged = cbElementForegroundChange
        end
        object cbElementBackground: TSpTBXColorEdit
          Left = 15
          Top = 229
          Width = 191
          Height = 23
          TabOrder = 2
          SelectedColor = clBlack
          SelectedFormat = cttHTML
          OnSelectedColorChanged = cbElementBackgroundChange
        end
      end
      object Options: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 589
        Height = 343
        Caption = 'Options'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem2'
        object gbOptions: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 3
          Width = 577
          Height = 260
          Align = alTop
          Caption = 'Options'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          object GridPanel1: TGridPanel
            Left = 2
            Top = 17
            Width = 573
            Height = 241
            Align = alClient
            BevelOuter = bvNone
            ColumnCollection = <
              item
                Value = 50.000000000000000000
              end
              item
                Value = 50.000000000000010000
              end>
            ControlCollection = <
              item
                Column = 0
                Control = StackPanel1
                Row = 0
              end
              item
                Column = 1
                Control = StackPanel2
                Row = 0
              end>
            RowCollection = <
              item
                Value = 100.000000000000000000
              end>
            TabOrder = 0
            object StackPanel1: TStackPanel
              Left = 0
              Top = 0
              Width = 286
              Height = 241
              Align = alClient
              BevelOuter = bvNone
              ControlCollection = <
                item
                  Control = ckAutoIndent
                end
                item
                  Control = ckRightMouseMoves
                end
                item
                  Control = ckDragAndDropEditing
                end
                item
                  Control = ckEnhanceEndKey
                end
                item
                  Control = ckWordWrap
                end
                item
                  Control = ckEnhanceHomeKey
                end
                item
                  Control = ckAltSetsColumnMode
                end
                item
                  Control = ckTabsToSpaces
                end
                item
                  Control = ckKeepCaretX
                end
                item
                  Control = ckSmartTabDelete
                end
                item
                  Control = ckTabIndent
                end
                item
                  Control = ckSmartTabs
                end>
              HorizontalPositioning = sphpFill
              TabOrder = 0
              object ckAutoIndent: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 0
                Width = 280
                Height = 17
                Hint = 
                  'Will indent the caret on new lines with the same amount of leadi' +
                  'ng white space as the preceding line'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Auto indent'
                TabOrder = 11
              end
              object ckRightMouseMoves: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 19
                Width = 280
                Height = 17
                Hint = 
                  'When clicking with the right mouse for a popup menu, move the cu' +
                  'rsor to that location'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Right mouse moves cursor'
                TabOrder = 0
              end
              object ckDragAndDropEditing: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 38
                Width = 280
                Height = 17
                Hint = 
                  'Allows you to select a block of text and drag it within the docu' +
                  'ment to another location'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Drag and drop editing'
                TabOrder = 10
              end
              object ckEnhanceEndKey: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 57
                Width = 280
                Height = 17
                Hint = 'Enhances end key similar to JDeveloper'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Enhance End Key'
                TabOrder = 1
              end
              object ckWordWrap: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 76
                Width = 280
                Height = 17
                Hint = 'Enable wrapping of long lines'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Word wrap'
                TabOrder = 9
              end
              object ckEnhanceHomeKey: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 95
                Width = 280
                Height = 17
                Hint = 'enhances home key positioning, similar to visual studio'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Enhance Home Key'
                TabOrder = 2
              end
              object ckAltSetsColumnMode: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 114
                Width = 280
                Height = 17
                Hint = 
                  'Holding down the Alt Key will put the selection mode into column' +
                  'ar format'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Alt sets column mode'
                TabOrder = 8
              end
              object ckTabsToSpaces: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 133
                Width = 280
                Height = 17
                Hint = 'Converts a tab character to the number of spaces in Tab Width'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Tabs to spaces'
                TabOrder = 3
              end
              object ckKeepCaretX: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 152
                Width = 280
                Height = 17
                Hint = 
                  'When moving through lines the X position will always stay the sa' +
                  'me'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Maintain caret column'
                TabOrder = 7
              end
              object ckSmartTabDelete: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 171
                Width = 280
                Height = 17
                Hint = 'similar to Smart Tabs, but when you delete characters'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Smart tab delete'
                TabOrder = 4
              end
              object ckTabIndent: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 190
                Width = 280
                Height = 17
                Hint = 'Tab indents and Shift-Tab unindents'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Tab Indent'
                TabOrder = 6
              end
              object ckSmartTabs: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 209
                Width = 280
                Height = 17
                Hint = 
                  'When tabbing, the cursor will go to the next non-white space cha' +
                  'racter of the previous line'
                Margins.Top = 0
                Margins.Bottom = 0
                Caption = 'Smart tabs'
                TabOrder = 5
              end
            end
            object StackPanel2: TStackPanel
              Left = 286
              Top = 0
              Width = 287
              Height = 241
              Align = alClient
              BevelOuter = bvNone
              ControlCollection = <
                item
                  Control = ckHalfPageScroll
                end
                item
                  Control = ckTrimTrailingSpaces
                end
                item
                  Control = ckScrollByOneLess
                end
                item
                  Control = ckShowSpecialChars
                end
                item
                  Control = ckScrollPastEOF
                end
                item
                  Control = ckDisableScrollArrows
                end
                item
                  Control = ckScrollPastEOL
                end
                item
                  Control = ckGroupUndo
                end
                item
                  Control = ckShowScrollHint
                end
                item
                  Control = ckHideShowScrollbars
                end
                item
                  Control = ckScrollHintFollows
                end
                item
                  Control = ckShowLigatures
                end>
              HorizontalPositioning = sphpFill
              TabOrder = 1
              DesignSize = (
                287
                241)
              object ckHalfPageScroll: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 0
                Width = 281
                Height = 17
                Hint = 
                  'When scrolling with page-up and page-down commands, only scroll ' +
                  'a half page at a time'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Half page scroll'
                TabOrder = 10
              end
              object ckTrimTrailingSpaces: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 19
                Width = 281
                Height = 17
                Hint = 'Spaces at the end of lines will be trimmed and not saved'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Trim trailing spaces'
                TabOrder = 0
              end
              object ckScrollByOneLess: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 38
                Width = 281
                Height = 17
                Hint = 'Forces scrolling to be one less'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Scroll by one less'
                TabOrder = 9
              end
              object ckShowSpecialChars: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 57
                Width = 281
                Height = 17
                Hint = 'Shows line breaks, spaces and tabs using special symbols'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Show special chars'
                TabOrder = 1
              end
              object ckScrollPastEOF: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 76
                Width = 281
                Height = 17
                Hint = 'Allows the cursor to go past the end of file marker'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Scroll past end of file'
                TabOrder = 8
              end
              object ckDisableScrollArrows: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 95
                Width = 281
                Height = 17
                Hint = 
                  'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
                  'hat direction any more'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Disable scroll arrows'
                TabOrder = 2
              end
              object ckScrollPastEOL: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 114
                Width = 281
                Height = 17
                Hint = 
                  'Allows the cursor to go past the last character into the white s' +
                  'pace at the end of a line'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Scroll past end of line'
                TabOrder = 7
              end
              object ckGroupUndo: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 133
                Width = 281
                Height = 17
                Hint = 
                  'When undoing/redoing actions, handle all continuous changes of t' +
                  'he same kind in one call instead undoing/redoing each command se' +
                  'parately'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Group undo'
                TabOrder = 3
              end
              object ckShowScrollHint: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 152
                Width = 281
                Height = 17
                Hint = 
                  'Shows a hint of the visible line numbers when scrolling vertical' +
                  'ly'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Show scroll hint'
                TabOrder = 6
              end
              object ckHideShowScrollbars: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 171
                Width = 281
                Height = 17
                Hint = 
                  'if enabled, then the scrollbars will only show when necessary.  ' +
                  'If you have ScrollPastEOL, then it the horizontal bar will alway' +
                  's be there (it uses MaxLength instead)'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Hide scrollbars as necessary'
                TabOrder = 4
              end
              object ckScrollHintFollows: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 190
                Width = 281
                Height = 17
                Hint = 'The scroll hint follows the mouse when scrolling vertically'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Scroll hint follows mouse'
                TabOrder = 5
              end
              object ckShowLigatures: TCheckBox
                AlignWithMargins = True
                Left = 3
                Top = 209
                Width = 281
                Height = 17
                Hint = 
                  'Show ligatures (joined font glyphs) when supported by the used f' +
                  'ont'
                Margins.Top = 0
                Margins.Bottom = 0
                Anchors = [akTop, akRight]
                Caption = 'Show font ligatures'
                TabOrder = 11
              end
            end
          end
        end
        object gbCaret: TGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 267
          Width = 577
          Height = 69
          Align = alBottom
          Caption = 'Caret'
          TabOrder = 1
          object Label2: TLabel
            Left = 60
            Top = 19
            Width = 61
            Height = 15
            Caption = 'Insert caret:'
          end
          object Label4: TLabel
            Left = 60
            Top = 43
            Width = 83
            Height = 15
            Caption = 'Overwrite caret:'
          end
          object cInsertCaret: TComboBox
            Left = 207
            Top = 18
            Width = 176
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            Items.Strings = (
              'Vertical Line'
              'Horizontal Line'
              'Half Block'
              'Block')
          end
          object cOverwriteCaret: TComboBox
            Left = 207
            Top = 42
            Width = 176
            Height = 23
            Style = csDropDownList
            TabOrder = 1
            Items.Strings = (
              'Vertical Line'
              'Horizontal Line'
              'Half Block'
              'Block')
          end
        end
      end
      object Display: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 589
        Height = 343
        Caption = 'Display'
        ImageIndex = -1
        DesignSize = (
          589
          343)
        TabItem = 'SpTBXTabItem1'
        object gbRightEdge: TGroupBox
          Left = 299
          Top = 116
          Width = 271
          Height = 80
          Anchors = [akTop, akRight]
          Caption = 'Right Edge'
          TabOrder = 1
          DesignSize = (
            271
            80)
          object Label3: TLabel
            Left = 9
            Top = 45
            Width = 59
            Height = 15
            Caption = 'Edge color:'
          end
          object Label10: TLabel
            Left = 9
            Top = 19
            Width = 75
            Height = 15
            Caption = 'Edge Column:'
          end
          object eRightEdge: TEdit
            Left = 128
            Top = 18
            Width = 51
            Height = 23
            Anchors = [akTop, akRight]
            TabOrder = 0
            Text = '0'
          end
          object cbRightEdgeColor: TSpTBXColorEdit
            Left = 128
            Top = 45
            Width = 129
            Height = 23
            Anchors = [akTop, akRight]
            TabOrder = 1
            SelectedColor = clBlack
            SelectedFormat = cttHTML
          end
        end
        object gbGutter: TGroupBox
          AlignWithMargins = True
          Left = 15
          Top = 212
          Width = 555
          Height = 124
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Gutter'
          TabOrder = 0
          DesignSize = (
            555
            124)
          object Label1: TLabel
            Left = 287
            Top = 78
            Width = 66
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'Gutter color:'
          end
          object pnlGutterFontDisplay: TPanel
            Left = 363
            Top = 44
            Width = 181
            Height = 27
            Anchors = [akTop, akRight]
            TabOrder = 6
            object lblGutterFont: TLabel
              Left = 1
              Top = 1
              Width = 179
              Height = 25
              Align = alClient
              Alignment = taCenter
              Caption = 'Terminal 8pt'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Terminal'
              Font.Style = []
              ParentFont = False
            end
          end
          object cbGutterColor: TSpTBXColorEdit
            Left = 399
            Top = 76
            Width = 137
            Height = 23
            TabOrder = 7
            SelectedColor = clBlack
            SelectedFormat = cttHTML
          end
          object ckGutterAutosize: TCheckBox
            Left = 24
            Top = 38
            Width = 230
            Height = 21
            Caption = 'Autosize'
            TabOrder = 1
          end
          object ckGutterShowLineNumbers: TCheckBox
            Left = 24
            Top = 58
            Width = 230
            Height = 21
            Caption = 'Show line numbers'
            TabOrder = 2
          end
          object ckGutterShowLeaderZeros: TCheckBox
            Left = 24
            Top = 99
            Width = 230
            Height = 21
            Caption = 'Show leading zeros'
            TabOrder = 3
          end
          object ckGutterVisible: TCheckBox
            Left = 24
            Top = 18
            Width = 230
            Height = 21
            Caption = 'Visible'
            Checked = True
            State = cbChecked
            TabOrder = 0
          end
          object ckGutterStartAtZero: TCheckBox
            Left = 24
            Top = 78
            Width = 230
            Height = 21
            Caption = 'Start at zero'
            TabOrder = 8
          end
          object cbGutterFont: TCheckBox
            Left = 287
            Top = 18
            Width = 156
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Use Gutter Font'
            TabOrder = 4
            OnClick = cbGutterFontClick
          end
          object ckGutterGradient: TCheckBox
            Left = 287
            Top = 99
            Width = 230
            Height = 21
            Hint = 'Gutter gradient visible'
            Anchors = [akTop, akRight]
            Caption = 'Gutter Gradient'
            TabOrder = 9
          end
          object btnGutterFont: TButton
            Left = 447
            Top = 15
            Width = 89
            Height = 25
            Caption = 'Font'
            TabOrder = 5
            OnClick = btnGutterFontClick
          end
        end
        object gbBookmarks: TGroupBox
          Left = 299
          Top = 50
          Width = 271
          Height = 62
          Anchors = [akTop, akRight]
          Caption = 'Bookmarks'
          TabOrder = 3
          object ckBookmarkKeys: TCheckBox
            Left = 23
            Top = 16
            Width = 230
            Height = 21
            Caption = 'Bookmark keys'
            TabOrder = 0
          end
          object ckBookmarkVisible: TCheckBox
            Left = 23
            Top = 35
            Width = 230
            Height = 21
            Caption = 'Bookmarks visible'
            TabOrder = 1
          end
        end
        object gbEditorFont: TGroupBox
          Left = 15
          Top = 10
          Width = 272
          Height = 92
          Caption = 'Editor Font'
          TabOrder = 4
          object Panel3: TPanel
            Left = 35
            Top = 21
            Width = 190
            Height = 30
            TabOrder = 1
            object labFont: TLabel
              Left = 1
              Top = 1
              Width = 188
              Height = 28
              Align = alClient
              Alignment = taCenter
              Caption = 'Courier New 10pt'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Courier New'
              Font.Style = []
              ParentFont = False
            end
          end
          object btnFont: TButton
            Left = 78
            Top = 57
            Width = 92
            Height = 25
            Caption = 'Font'
            TabOrder = 0
            OnClick = btnFontClick
          end
        end
        object gbLineSpacing: TGroupBox
          Left = 15
          Top = 108
          Width = 272
          Height = 88
          Anchors = [akTop]
          Caption = 'Line spacing / Tab spacing'
          TabOrder = 2
          DesignSize = (
            272
            88)
          object Label8: TLabel
            Left = 23
            Top = 27
            Width = 136
            Height = 15
            Caption = 'Extra line spacing (pixels):'
          end
          object Label9: TLabel
            Left = 23
            Top = 56
            Width = 56
            Height = 15
            Caption = 'Tab Width:'
          end
          object eLineSpacing: TEdit
            Left = 208
            Top = 23
            Width = 42
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = '0'
          end
          object eTabWidth: TEdit
            Left = 208
            Top = 53
            Width = 42
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Text = '8'
          end
        end
        object GroupBox2: TGroupBox
          Left = 299
          Top = 7
          Width = 271
          Height = 43
          Anchors = [akTop, akRight]
          Caption = 'Active Line Color'
          TabOrder = 5
          DesignSize = (
            271
            43)
          object cbActiveLineColor: TSpTBXColorEdit
            Left = 48
            Top = 16
            Width = 206
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            SelectedColor = clBlack
            SelectedFormat = cttHTML
          end
        end
      end
    end
    object cbApplyToAll: TCheckBox
      Left = 19
      Top = 390
      Width = 230
      Height = 21
      Caption = 'Apply to all editors'
      Checked = True
      Color = clNone
      ParentColor = False
      State = cbChecked
      TabOrder = 2
    end
    object btnOk: TButton
      Left = 281
      Top = 388
      Width = 94
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 385
      Top = 388
      Width = 94
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 489
      Top = 388
      Width = 94
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 3
      OnClick = btnHelpClick
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly, fdScalableOnly]
    Left = 386
    Top = 5
  end
end
