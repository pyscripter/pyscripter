inherited fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 521
  Top = 154
  HelpContext = 620
  Anchors = [akRight, akBottom]
  Caption = 'Editor Options'
  ClientHeight = 421
  ClientWidth = 492
  OldCreateOrder = True
  ShowHint = True
  OnShow = FormShow
  ExplicitWidth = 498
  ExplicitHeight = 449
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 492
    Height = 421
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      492
      421)
    object btnOk: TSpTBXButton
      Left = 249
      Top = 389
      Width = 73
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 0
      OnClick = btnOkClick
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 330
      Top = 389
      Width = 73
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Cancel = True
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 409
      Top = 388
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object cbApplyToAll: TSpTBXCheckBox
      Left = 19
      Top = 366
      Width = 113
      Height = 21
      Caption = 'Apply to all editors'
      ParentColor = True
      TabOrder = 2
      Checked = True
      State = cbChecked
    end
    object TabControl: TSpTBXTabControl
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 482
      Height = 353
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
      object SpTBXTabItem4: TSpTBXTabItem
        Caption = 'Syntax Colors'
      end
      object KeyStrokes: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 482
        Height = 328
        Caption = 'Keystrokes'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem3'
        object gbKeyStrokes: TSpTBXGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 202
          Width = 470
          Height = 119
          Caption = 'Keystroke Options'
          Align = alBottom
          TabOrder = 3
          TBXStyleBackground = True
          object Label5: TSpTBXLabel
            Left = 50
            Top = 28
            Width = 57
            Height = 19
            Caption = 'Command:'
          end
          object Label6: TSpTBXLabel
            Left = 50
            Top = 91
            Width = 58
            Height = 19
            Caption = 'Keystroke:'
          end
          object Label7: TSpTBXLabel
            Left = 50
            Top = 59
            Width = 58
            Height = 19
            Caption = 'Keystroke:'
          end
          object cKeyCommand: TSpTBXComboBox
            Left = 154
            Top = 23
            Width = 186
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnExit = cKeyCommandExit
            OnKeyPress = cKeyCommandKeyPress
            OnKeyUp = cKeyCommandKeyUp
          end
        end
        object btnAddKey: TSpTBXButton
          Left = 203
          Top = 166
          Width = 75
          Height = 25
          Caption = '&Add'
          TabOrder = 1
          OnClick = btnAddKeyClick
        end
        object btnRemKey: TSpTBXButton
          Left = 284
          Top = 166
          Width = 75
          Height = 25
          Caption = '&Remove'
          TabOrder = 2
          OnClick = btnRemKeyClick
        end
        object btnUpdateKey: TSpTBXButton
          Left = 122
          Top = 166
          Width = 75
          Height = 25
          Caption = '&Update'
          TabOrder = 0
          OnClick = btnUpdateKeyClick
        end
        object SpTBXPanel2: TSpTBXPanel
          Left = 2
          Top = 0
          Width = 476
          Height = 158
          Align = alTop
          TabOrder = 4
          HotTrack = True
          object KeyList: TEasyListview
            AlignWithMargins = True
            Left = 5
            Top = 5
            Width = 466
            Height = 148
            Align = alClient
            EditManager.Font.Charset = DEFAULT_CHARSET
            EditManager.Font.Color = clWindowText
            EditManager.Font.Height = -11
            EditManager.Font.Name = 'Tahoma'
            EditManager.Font.Style = []
            Header.Columns.Items = {
              0600000002000000110000005445617379436F6C756D6E53746F726564FFFECE
              00060000008008000101000100000000010001A7000000FFFFFF1F0001000000
              010000000700000043006F006D006D0061006E00640000000000000000000000
              0000110000005445617379436F6C756D6E53746F726564FFFECE000600000080
              080001010001010000000000018E000000FFFFFF1F0001000000010000000900
              00004B00650079007300740072006F006B006500000000000000000000000000}
            Header.Draggable = False
            Header.Visible = True
            HotTrack.Enabled = True
            PaintInfoGroup.MarginBottom.CaptionIndent = 4
            Selection.FullRowSelect = True
            TabOrder = 0
            View = elsReport
          end
        end
      end
      object Color: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 482
        Height = 328
        Caption = 'Syntax Colors'
        ImageIndex = -1
        DesignSize = (
          482
          328)
        TabItem = 'SpTBXTabItem4'
        object GroupBox1: TSpTBXGroupBox
          Left = 6
          Top = 256
          Width = 150
          Height = 62
          Caption = ' Text attributes '
          TabOrder = 3
          TBXStyleBackground = True
          object cbxElementBold: TSpTBXCheckBox
            Left = 9
            Top = 17
            Width = 44
            Height = 21
            Caption = '&Bold'
            Enabled = False
            ParentColor = True
            TabOrder = 0
            OnClick = cbxElementBoldClick
          end
          object cbxElementItalic: TSpTBXCheckBox
            Left = 9
            Top = 40
            Width = 47
            Height = 21
            Caption = '&Italic'
            Enabled = False
            ParentColor = True
            TabOrder = 1
            OnClick = cbxElementBoldClick
          end
          object cbxElementUnderline: TSpTBXCheckBox
            Left = 67
            Top = 17
            Width = 69
            Height = 21
            Caption = '&Underline'
            Enabled = False
            ParentColor = True
            TabOrder = 2
            OnClick = cbxElementBoldClick
          end
          object cbxElementStrikeout: TSpTBXCheckBox
            Left = 67
            Top = 40
            Width = 72
            Height = 21
            Caption = '&Strike Out'
            Enabled = False
            ParentColor = True
            TabOrder = 3
            OnClick = cbxElementBoldClick
          end
        end
        object SynEdit1: TSynEdit
          Left = 166
          Top = 64
          Width = 313
          Height = 257
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
          Left = 6
          Top = 45
          Width = 48
          Height = 19
          Caption = '&Element:'
        end
        object Label12: TSpTBXLabel
          Left = 6
          Top = 164
          Width = 94
          Height = 19
          Caption = '&Foreground Color:'
        end
        object Label13: TSpTBXLabel
          Left = 6
          Top = 208
          Width = 94
          Height = 19
          Caption = 'B&ackground Color:'
        end
        object Label14: TSpTBXLabel
          Left = 164
          Top = 45
          Width = 68
          Height = 19
          Caption = 'Code Sample'
        end
        object Label15: TSpTBXLabel
          Left = 6
          Top = 2
          Width = 121
          Height = 19
          Caption = 'Editor Syntax Language'
        end
        object cbHighlighters: TSpTBXComboBox
          Left = 3
          Top = 23
          Width = 467
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 5
          OnChange = cbHighlightersChange
        end
        object lbElements: TSpTBXListBox
          Left = 6
          Top = 64
          Width = 150
          Height = 97
          Style = lbStandard
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbElementsClick
        end
        object cbElementForeground: TSpTBXColorEdit
          Left = 6
          Top = 181
          Width = 154
          Height = 21
          TabOrder = 1
          SelectedColor = clBlack
          OnSelectedColorChanged = cbElementForegroundChange
        end
        object cbElementBackground: TSpTBXColorEdit
          Left = 6
          Top = 230
          Width = 151
          Height = 21
          TabOrder = 2
          SelectedColor = clBlack
          OnSelectedColorChanged = cbElementBackgroundChange
        end
      end
      object Options: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 482
        Height = 328
        Caption = 'Options'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem2'
        object gbOptions: TSpTBXGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 3
          Width = 470
          Height = 247
          Caption = 'Options'
          Align = alTop
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          TBXStyleBackground = True
          DesignSize = (
            470
            247)
          object ckAutoIndent: TSpTBXCheckBox
            Left = 23
            Top = 15
            Width = 80
            Height = 21
            Hint = 
              'Will indent the caret on new lines with the same amount of leadi' +
              'ng white space as the preceding line'
            Caption = 'Auto indent'
            ParentColor = True
            TabOrder = 0
          end
          object ckDragAndDropEditing: TSpTBXCheckBox
            Left = 23
            Top = 34
            Width = 128
            Height = 21
            Hint = 
              'Allows you to select a block of text and drag it within the docu' +
              'ment to another location'
            Caption = 'Drag and drop editing'
            ParentColor = True
            TabOrder = 1
          end
          object ckWordWrap: TSpTBXCheckBox
            Left = 23
            Top = 53
            Width = 77
            Height = 21
            Hint = 'Allows the editor accept OLE file drops'
            Caption = 'Word wrap'
            ParentColor = True
            TabOrder = 2
          end
          object ckHalfPageScroll: TSpTBXCheckBox
            Left = 224
            Top = 15
            Width = 97
            Height = 21
            Hint = 
              'When scrolling with page-up and page-down commands, only scroll ' +
              'a half page at a time'
            Caption = 'Half page scroll'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 11
          end
          object ckThemeSelection: TSpTBXCheckBox
            Left = 224
            Top = 224
            Width = 101
            Height = 21
            Hint = 'Themes the selection color'
            Caption = 'Theme selection'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 20
          end
          object ckScrollByOneLess: TSpTBXCheckBox
            Left = 224
            Top = 34
            Width = 106
            Height = 21
            Hint = 'Forces scrolling to be one less'
            Caption = 'Scroll by one less'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 12
          end
          object ckScrollPastEOF: TSpTBXCheckBox
            Left = 224
            Top = 53
            Width = 124
            Height = 21
            Hint = 'Allows the cursor to go past the end of file marker'
            Caption = 'Scroll past end of file'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 13
          end
          object ckScrollPastEOL: TSpTBXCheckBox
            Left = 224
            Top = 72
            Width = 126
            Height = 21
            Hint = 
              'Allows the cursor to go past the last character into the white s' +
              'pace at the end of a line'
            Caption = 'Scroll past end of line'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 14
          end
          object ckShowScrollHint: TSpTBXCheckBox
            Left = 224
            Top = 91
            Width = 98
            Height = 21
            Hint = 
              'Shows a hint of the visible line numbers when scrolling vertical' +
              'ly'
            Caption = 'Show scroll hint'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 15
          end
          object ckSmartTabs: TSpTBXCheckBox
            Left = 23
            Top = 129
            Width = 76
            Height = 21
            Hint = 
              'When tabbing, the cursor will go to the next non-white space cha' +
              'racter of the previous line'
            Caption = 'Smart tabs'
            ParentColor = True
            TabOrder = 6
          end
          object ckTabsToSpaces: TSpTBXCheckBox
            Left = 23
            Top = 167
            Width = 96
            Height = 21
            Hint = 'Converts a tab character to the number of spaces in Tab Width'
            Caption = 'Tabs to spaces'
            ParentColor = True
            TabOrder = 17
          end
          object ckTrimTrailingSpaces: TSpTBXCheckBox
            Left = 224
            Top = 148
            Width = 115
            Height = 21
            Hint = 'Spaces at the end of lines will be trimmed and not saved'
            Caption = 'Trim trailing spaces'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 18
          end
          object ckTabIndent: TSpTBXCheckBox
            Left = 23
            Top = 110
            Width = 77
            Height = 21
            Hint = 'Tab indents and Shft-Tab unindents'
            Caption = 'Tab Indent'
            ParentColor = True
            TabOrder = 5
          end
          object ckAltSetsColumnMode: TSpTBXCheckBox
            Left = 23
            Top = 72
            Width = 125
            Height = 21
            Hint = 
              'Holding down the Alt Key will put the selection mode into column' +
              'ar format'
            Caption = 'Alt sets column mode'
            ParentColor = True
            TabOrder = 3
          end
          object ckKeepCaretX: TSpTBXCheckBox
            Left = 23
            Top = 91
            Width = 128
            Height = 21
            Hint = 
              'When moving through lines the X position will always stay the sa' +
              'me'
            Caption = 'Maintain caret column'
            ParentColor = True
            TabOrder = 4
          end
          object ckScrollHintFollows: TSpTBXCheckBox
            Left = 224
            Top = 110
            Width = 140
            Height = 21
            Hint = 'The scroll hint follows the mouse when scrolling vertically'
            Caption = 'Scroll hint follows mouse'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 16
          end
          object ckGroupUndo: TSpTBXCheckBox
            Left = 224
            Top = 167
            Width = 80
            Height = 21
            Hint = 
              'When undoing/redoing actions, handle all continous changes of th' +
              'e same kind in one call instead undoing/redoing each command sep' +
              'arately'
            Caption = 'Group undo'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 19
          end
          object ckSmartTabDelete: TSpTBXCheckBox
            Left = 23
            Top = 148
            Width = 104
            Height = 21
            Hint = 'similar to Smart Tabs, but when you delete characters'
            Caption = 'Smart tab delete'
            ParentColor = True
            TabOrder = 7
          end
          object ckRightMouseMoves: TSpTBXCheckBox
            Left = 23
            Top = 186
            Width = 150
            Height = 21
            Hint = 
              'When clicking with the right mouse for a popup menu, move the cu' +
              'rsor to that location'
            Caption = 'Right mouse moves cursor'
            ParentColor = True
            TabOrder = 8
          end
          object ckEnhanceHomeKey: TSpTBXCheckBox
            Left = 23
            Top = 205
            Width = 116
            Height = 21
            Hint = 'enhances home key positioning, similar to visual studio'
            Caption = 'Enhance Home Key'
            ParentColor = True
            TabOrder = 9
          end
          object ckHideShowScrollbars: TSpTBXCheckBox
            Left = 224
            Top = 129
            Width = 159
            Height = 21
            Hint = 
              'if enabled, then the scrollbars will only show when necessary.  ' +
              'If you have ScrollPastEOL, then it the horizontal bar will alway' +
              's be there (it uses MaxLength instead)'
            Caption = 'Hide scrollbars as necessary'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 10
          end
          object ckDisableScrollArrows: TSpTBXCheckBox
            Left = 224
            Top = 186
            Width = 121
            Height = 21
            Hint = 
              'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
              'hat direction any more'
            Caption = 'Disable scroll arrows'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 21
          end
          object ckShowSpecialChars: TSpTBXCheckBox
            Left = 224
            Top = 205
            Width = 114
            Height = 21
            Hint = 'Shows linebreaks, spaces and tabs using special symbols'
            Caption = 'Show special chars'
            Anchors = [akTop, akRight]
            ParentColor = True
            TabOrder = 22
          end
          object ckEnhanceEndKey: TSpTBXCheckBox
            Left = 23
            Top = 224
            Width = 107
            Height = 21
            Hint = 'Enhances end key similar to JDeveloper'
            Caption = 'Enhance End Key'
            ParentColor = True
            TabOrder = 23
          end
        end
        object gbCaret: TSpTBXGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 252
          Width = 470
          Height = 69
          Caption = 'Caret'
          Align = alBottom
          TabOrder = 1
          TBXStyleBackground = True
          object Label2: TSpTBXLabel
            Left = 60
            Top = 19
            Width = 67
            Height = 19
            Caption = 'Insert caret:'
          end
          object Label4: TSpTBXLabel
            Left = 60
            Top = 43
            Width = 86
            Height = 19
            Caption = 'Overwrite caret:'
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
          end
        end
      end
      object Display: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 482
        Height = 328
        Caption = 'Display'
        ImageIndex = -1
        DesignSize = (
          482
          328)
        TabItem = 'SpTBXTabItem1'
        object gbRightEdge: TSpTBXGroupBox
          Left = 240
          Top = 116
          Width = 235
          Height = 80
          Caption = 'Right Edge'
          Anchors = [akTop, akRight]
          TabOrder = 1
          TBXStyleBackground = True
          object Label3: TSpTBXLabel
            Left = 9
            Top = 45
            Width = 60
            Height = 19
            Caption = 'Edge color:'
          end
          object Label10: TSpTBXLabel
            Left = 9
            Top = 20
            Width = 72
            Height = 19
            Caption = 'Edge Column:'
          end
          object eRightEdge: TSpTBXEdit
            Left = 82
            Top = 17
            Width = 51
            Height = 21
            TabOrder = 0
            Text = '0'
          end
          object cbRightEdgeColor: TSpTBXColorEdit
            Left = 75
            Top = 45
            Width = 132
            Height = 21
            TabOrder = 1
            SelectedColor = clBlack
          end
        end
        object gbGutter: TSpTBXGroupBox
          AlignWithMargins = True
          Left = 5
          Top = 197
          Width = 470
          Height = 124
          Caption = 'Gutter'
          Align = alBottom
          TabOrder = 0
          TBXStyleBackground = True
          object pnlGutterFontDisplay: TSpTBXPanel
            Left = 270
            Top = 44
            Width = 175
            Height = 27
            TabOrder = 6
            TBXStyleBackground = True
            object lblGutterFont: TSpTBXLabel
              Left = 2
              Top = 2
              Width = 171
              Height = 23
              Caption = 'Terminal 8pt'
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Terminal'
              Font.Style = []
              ParentFont = False
              Alignment = taCenter
            end
          end
          object btnGutterFont: TSpTBXButton
            Left = 346
            Top = 13
            Width = 99
            Height = 25
            Caption = 'Font'
            TabOrder = 5
            OnClick = btnGutterFontClick
          end
          object ckGutterAutosize: TSpTBXCheckBox
            Left = 23
            Top = 38
            Width = 65
            Height = 21
            Caption = 'Autosize'
            ParentColor = True
            TabOrder = 1
          end
          object ckGutterShowLineNumbers: TSpTBXCheckBox
            Left = 23
            Top = 59
            Width = 113
            Height = 21
            Caption = 'Show line numbers'
            ParentColor = True
            TabOrder = 2
          end
          object ckGutterShowLeaderZeros: TSpTBXCheckBox
            Left = 23
            Top = 100
            Width = 116
            Height = 21
            Caption = 'Show leading zeros'
            ParentColor = True
            TabOrder = 3
          end
          object ckGutterVisible: TSpTBXCheckBox
            Left = 23
            Top = 18
            Width = 53
            Height = 21
            Caption = 'Visible'
            ParentColor = True
            TabOrder = 0
            Checked = True
            State = cbChecked
          end
          object cbGutterFont: TSpTBXCheckBox
            Left = 233
            Top = 17
            Width = 101
            Height = 21
            Caption = 'Use Gutter Font'
            ParentColor = True
            TabOrder = 4
            OnClick = cbGutterFontClick
          end
          object ckGutterStartAtZero: TSpTBXCheckBox
            Left = 23
            Top = 79
            Width = 85
            Height = 21
            Caption = 'Start at zero'
            ParentColor = True
            TabOrder = 8
          end
          object ckGutterGradient: TSpTBXCheckBox
            Left = 237
            Top = 98
            Width = 99
            Height = 21
            Hint = 'Gutter gradient visible'
            Caption = 'Gutter Gradient'
            ParentColor = True
            TabOrder = 9
          end
          object Label1: TSpTBXLabel
            Left = 237
            Top = 77
            Width = 67
            Height = 19
            Caption = 'Gutter color:'
          end
          object cbGutterColor: TSpTBXColorEdit
            Left = 310
            Top = 75
            Width = 132
            Height = 21
            TabOrder = 7
            SelectedColor = clBlack
          end
        end
        object gbBookmarks: TSpTBXGroupBox
          Left = 238
          Top = 50
          Width = 237
          Height = 62
          Caption = 'Bookmarks'
          TabOrder = 3
          TBXStyleBackground = True
          object ckBookmarkKeys: TSpTBXCheckBox
            Left = 23
            Top = 16
            Width = 95
            Height = 21
            Caption = 'Bookmark keys'
            ParentColor = True
            TabOrder = 0
          end
          object ckBookmarkVisible: TSpTBXCheckBox
            Left = 23
            Top = 35
            Width = 107
            Height = 21
            Caption = 'Bookmarks visible'
            ParentColor = True
            TabOrder = 1
          end
        end
        object gbEditorFont: TSpTBXGroupBox
          Left = 9
          Top = 10
          Width = 223
          Height = 92
          Caption = 'Editor Font'
          TabOrder = 4
          TBXStyleBackground = True
          object Panel3: TSpTBXPanel
            Left = 19
            Top = 21
            Width = 190
            Height = 30
            TabOrder = 1
            TBXStyleBackground = True
            object labFont: TSpTBXLabel
              Left = 2
              Top = 2
              Width = 186
              Height = 26
              Caption = 'Courier New 10pt'
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Courier New'
              Font.Style = []
              ParentFont = False
              Alignment = taCenter
            end
          end
          object btnFont: TSpTBXButton
            Left = 62
            Top = 57
            Width = 92
            Height = 25
            Caption = 'Font'
            TabOrder = 0
            OnClick = btnFontClick
          end
        end
        object gbLineSpacing: TSpTBXGroupBox
          Left = 6
          Top = 108
          Width = 226
          Height = 88
          Caption = 'Line spacing / Tab spacing'
          Anchors = [akTop]
          TabOrder = 2
          TBXStyleBackground = True
          object Label8: TSpTBXLabel
            Left = 23
            Top = 27
            Width = 63
            Height = 19
            Caption = 'Extra Lines:'
          end
          object Label9: TSpTBXLabel
            Left = 23
            Top = 56
            Width = 59
            Height = 19
            Caption = 'Tab Width:'
          end
          object eLineSpacing: TSpTBXEdit
            Left = 116
            Top = 23
            Width = 48
            Height = 21
            TabOrder = 0
            Text = '0'
          end
          object eTabWidth: TSpTBXEdit
            Left = 116
            Top = 53
            Width = 48
            Height = 21
            TabOrder = 1
            Text = '8'
          end
        end
        object GroupBox2: TSpTBXGroupBox
          Left = 238
          Top = 7
          Width = 237
          Height = 43
          Caption = 'Active Line Color'
          Anchors = [akTop, akRight]
          TabOrder = 5
          TBXStyleBackground = True
          object cbActiveLineColor: TSpTBXColorEdit
            Left = 41
            Top = 15
            Width = 152
            Height = 21
            TabOrder = 0
            SelectedColor = clBlack
          end
        end
      end
    end
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
