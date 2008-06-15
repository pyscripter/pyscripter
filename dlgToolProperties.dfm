inherited ToolProperties: TToolProperties
  Left = 334
  Top = 153
  HelpContext = 710
  Caption = 'External Tool Properties'
  ClientHeight = 498
  ClientWidth = 406
  Font.Name = 'MS Shell Dlg 2'
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 412
  ExplicitHeight = 524
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 406
    Height = 498
    ThemeType = thtWindows
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    DesignSize = (
      406
      498)
    object PageControl: TPageControl
      Left = 2
      Top = 2
      Width = 402
      Height = 453
      ActivePage = tsProperties
      Align = alTop
      TabOrder = 0
      object tsProperties: TTabSheet
        Caption = '&Properties'
        object GroupBox1: TSpTBXGroupBox
          Left = 4
          Top = 5
          Width = 385
          Height = 65
          Caption = 'General'
          ThemeType = thtWindows
          TabOrder = 0
          object Label1: TSpTBXLabel
            Left = 8
            Top = 16
            Width = 31
            Height = 13
            Caption = '&Name:'
            ParentColor = True
            FocusControl = edName
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label5: TSpTBXLabel
            Left = 8
            Top = 39
            Width = 57
            Height = 13
            Caption = 'Desc&ription:'
            ParentColor = True
            FocusControl = edDescription
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label17: TSpTBXLabel
            Left = 200
            Top = 17
            Width = 48
            Height = 13
            Caption = '(required)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object edName: TSpTBXEdit
            Left = 71
            Top = 13
            Width = 118
            Height = 21
            TabOrder = 0
            ThemeType = thtWindows
          end
          object edDescription: TSpTBXEdit
            Left = 71
            Top = 36
            Width = 307
            Height = 21
            TabOrder = 1
          end
        end
        object GroupBox2: TSpTBXGroupBox
          Left = 4
          Top = 73
          Width = 385
          Height = 108
          Caption = 'Command'
          ThemeType = thtWindows
          TabOrder = 1
          object SynApplication: TSynEdit
            Left = 71
            Top = 18
            Width = 283
            Height = 18
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 0
            OnEnter = SynApplicationEnter
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
            HideSelection = True
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            WantReturns = False
          end
          object SynParameters: TSynEdit
            Left = 71
            Top = 40
            Width = 302
            Height = 18
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 2
            OnEnter = SynParametersEnter
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
            HideSelection = True
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            WantReturns = False
          end
          object SynWorkDir: TSynEdit
            Left = 114
            Top = 64
            Width = 239
            Height = 18
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 3
            OnEnter = SynWorkDirEnter
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
            HideSelection = True
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            WantReturns = False
          end
          object btnAppDir: TSpTBXButton
            Left = 357
            Top = 15
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 1
            OnClick = btnAppDirClick
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object btnWorkDir: TSpTBXButton
            Left = 357
            Top = 63
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 4
            OnClick = btnWorkDirClick
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label2: TSpTBXLabel
            Left = 6
            Top = 18
            Width = 56
            Height = 13
            Caption = '&Application:'
            ParentColor = True
            FocusControl = SynApplication
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label6: TSpTBXLabel
            Left = 6
            Top = 42
            Width = 59
            Height = 13
            Caption = '&Parameters:'
            ParentColor = True
            FocusControl = SynParameters
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label7: TSpTBXLabel
            Left = 6
            Top = 66
            Width = 89
            Height = 13
            Caption = 'Working &directory:'
            ParentColor = True
            FocusControl = SynWorkDir
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label3: TSpTBXLabel
            Left = 6
            Top = 90
            Width = 252
            Height = 13
            Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
        end
        object GroupBox4: TSpTBXGroupBox
          Left = 5
          Top = 187
          Width = 385
          Height = 64
          Caption = 'Shortcut and Context'
          ThemeType = thtWindows
          TabOrder = 2
          object lbShortcut: TSpTBXLabel
            Left = 11
            Top = 18
            Width = 45
            Height = 13
            Caption = '&Shortcut:'
            ParentColor = True
            FocusControl = hkShortCut
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object lbContext: TSpTBXLabel
            Left = 13
            Top = 40
            Width = 43
            Height = 13
            Caption = 'Conte&xt:'
            ParentColor = True
            FocusControl = cbContext
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label13: TSpTBXLabel
            Left = 232
            Top = 18
            Width = 50
            Height = 13
            Caption = 'Sa&ve files:'
            ParentColor = True
            FocusControl = cbSaveFiles
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object hkShortCut: THotKey
            Left = 86
            Top = 15
            Width = 125
            Height = 19
            Hint = 'Allows you to specify a menu shortcut for the tool'
            HotKey = 0
            InvalidKeys = [hcNone]
            Modifiers = []
            TabOrder = 0
          end
          object cbContext: TSpTBXComboBox
            Left = 86
            Top = 36
            Width = 125
            Height = 21
            Hint = 'Specifies the context in which'#13#10'the tool is enabled.'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            Items.Strings = (
              'Always enabled'
              'Active editor'
              'Active Python file'
              'Selection available')
            ThemeType = thtWindows
          end
          object cbSaveFiles: TSpTBXComboBox
            Left = 233
            Top = 37
            Width = 142
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
            Items.Strings = (
              'None'
              'Active only'
              'All files')
            ThemeType = thtWindows
          end
        end
        object GroupBox3: TSpTBXGroupBox
          Left = 5
          Top = 258
          Width = 385
          Height = 94
          Caption = 'Standard Input and Output'
          ThemeType = thtWindows
          TabOrder = 3
          object btnStdFormats: TSpTBXButton
            Left = 357
            Top = 66
            Width = 19
            Height = 20
            Caption = '<'
            TabOrder = 6
            DropDownArrow = False
            DropDownMenu = FormatsPopup
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object cbCaptureOutput: TSpTBXCheckBox
            Left = 233
            Top = 13
            Width = 94
            Height = 15
            Caption = '&Capture Output'
            ParentColor = True
            TabOrder = 2
            Checked = True
            State = cbChecked
            ThemeType = thtWindows
          end
          object cbParseMessages: TSpTBXCheckBox
            Left = 233
            Top = 30
            Width = 95
            Height = 15
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Messages'
            ParentColor = True
            TabOrder = 3
            OnClick = cbParseMessagesClick
            Checked = True
            State = cbChecked
            ThemeType = thtWindows
          end
          object cbParseTraceback: TSpTBXCheckBox
            Left = 233
            Top = 47
            Width = 97
            Height = 15
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Traceback'
            ParentColor = True
            TabOrder = 4
            Checked = True
            State = cbChecked
            ThemeType = thtWindows
          end
          object Label10: TSpTBXLabel
            Left = 6
            Top = 18
            Width = 53
            Height = 13
            Caption = 'Std. &Input:'
            ParentColor = True
            FocusControl = cbStandardInput
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label11: TSpTBXLabel
            Left = 6
            Top = 42
            Width = 61
            Height = 13
            Caption = 'Std. &Output:'
            ParentColor = True
            FocusControl = cbStandardOutput
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label12: TSpTBXLabel
            Left = 6
            Top = 70
            Width = 88
            Height = 13
            Caption = 'Messages &Format:'
            ParentColor = True
            FocusControl = edMessagesFormat
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object edMessagesFormat: TSpTBXEdit
            Left = 113
            Top = 67
            Width = 240
            Height = 21
            Hint = 
              'A grep expression for parsing output lines.'#13#10'Use the button on t' +
              'he right to insert '#13#10'predefined grep expressions.'
            AutoSelect = False
            AutoSize = False
            TabOrder = 5
            ThemeType = thtWindows
          end
          object cbStandardInput: TSpTBXComboBox
            Left = 86
            Top = 16
            Width = 125
            Height = 21
            Hint = 
              'Select option other than None to '#13#10'feed the corresponding inform' +
              'ation'#13#10'to the standard input.'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            Items.Strings = (
              'None'
              'Word at cursor'
              'Current line'
              'Selection'
              'ActiveFile')
            ThemeType = thtWindows
          end
          object cbStandardOutput: TSpTBXComboBox
            Left = 86
            Top = 40
            Width = 125
            Height = 21
            Hint = 
              'Select option other than None to '#13#10'redirect the output to the '#13#10 +
              'corresponding outlet.'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            Items.Strings = (
              'None'
              'Word at cursor'
              'Current line'
              'Selection'
              'Active file'
              'New file')
            ThemeType = thtWindows
          end
        end
        object GroupBox5: TSpTBXGroupBox
          Left = 6
          Top = 352
          Width = 385
          Height = 64
          Caption = 'Console'
          ThemeType = thtWindows
          TabOrder = 4
          object cbHideConsole: TSpTBXCheckBox
            Left = 8
            Top = 16
            Width = 78
            Height = 15
            Hint = 'Check to hide the console.'
            Caption = '&Hide console'
            ParentColor = True
            TabOrder = 0
            Checked = True
            State = cbChecked
            ThemeType = thtWindows
          end
          object cbWaitForTermination: TSpTBXCheckBox
            Left = 8
            Top = 37
            Width = 116
            Height = 15
            Hint = 
              'If checked will monitor the process '#13#10'and notify you when is ter' +
              'minated.'
            Caption = '&Wait for Termination'
            ParentColor = True
            TabOrder = 1
            Checked = True
            State = cbChecked
            ThemeType = thtWindows
          end
          object Label9: TSpTBXLabel
            Left = 239
            Top = 18
            Width = 66
            Height = 13
            Caption = '&Timeout (ms):'
            ParentColor = True
            FocusControl = seTimeout
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object seTimeout: TSpTBXSpinEdit
            Left = 240
            Top = 37
            Width = 79
            Height = 21
            Hint = 
              'If set to a value <> 0 then you will be prompted '#13#10'to abort the ' +
              'program after the specified time.'
            TabOrder = 2
            Alignment = taLeftJustify
            ThemeType = thtWindows
            Increment = 100.000000000000000000
            SpinButton.Left = 60
            SpinButton.Top = 0
            SpinButton.Width = 15
            SpinButton.Height = 17
            SpinButton.Align = alRight
            SpinButton.LinkFont.Charset = DEFAULT_CHARSET
            SpinButton.LinkFont.Color = clBlue
            SpinButton.LinkFont.Height = -11
            SpinButton.LinkFont.Name = 'Tahoma'
            SpinButton.LinkFont.Style = [fsUnderline]
            SpinButton.ThemeType = thtWindows
          end
        end
      end
      object tsEnvironment: TTabSheet
        Caption = '&Environment'
        ImageIndex = 1
        object GroupBox6: TSpTBXGroupBox
          Left = 3
          Top = 307
          Width = 389
          Height = 83
          Caption = 'Name-Value Pair'
          ThemeType = thtWindows
          TabOrder = 1
          object Label15: TSpTBXLabel
            Left = 14
            Top = 28
            Width = 31
            Height = 13
            Caption = '&Name:'
            ParentColor = True
            FocusControl = edEnvName
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object Label16: TSpTBXLabel
            Left = 14
            Top = 50
            Width = 30
            Height = 13
            Caption = '&Value:'
            ParentColor = True
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Shell Dlg 2'
            LinkFont.Style = [fsUnderline]
          end
          object edEnvName: TSpTBXEdit
            Left = 61
            Top = 23
            Width = 121
            Height = 21
            TabOrder = 0
            ThemeType = thtWindows
          end
          object edEnvValue: TSpTBXEdit
            Left = 62
            Top = 48
            Width = 308
            Height = 21
            Cursor = crIBeam
            HideSelection = False
            TabOrder = 1
            ThemeType = thtWindows
          end
        end
        object cbUseCustomEnv: TSpTBXCheckBox
          Left = 123
          Top = 401
          Width = 157
          Height = 15
          Caption = 'Use Customized Environment'
          ParentColor = True
          TabOrder = 2
          ThemeType = thtWindows
        end
        object btnAdd: TSpTBXButton
          Left = -1
          Top = 273
          Width = 77
          Height = 24
          Action = actAddItem
          TabOrder = 3
          Images = CommandsDataModule.Images
          ImageIndex = 49
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object btnDelete: TSpTBXButton
          Left = 77
          Top = 273
          Width = 77
          Height = 24
          Action = actDeleteItem
          TabOrder = 4
          Images = CommandsDataModule.Images
          ImageIndex = 14
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object btnMoveUp: TSpTBXButton
          Left = 156
          Top = 273
          Width = 77
          Height = 24
          Action = actMoveUp
          TabOrder = 5
          Images = CommandsDataModule.Images
          ImageIndex = 47
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object btnMoveDown: TSpTBXButton
          Left = 235
          Top = 273
          Width = 77
          Height = 24
          Action = actMoveDown
          TabOrder = 6
          Images = CommandsDataModule.Images
          ImageIndex = 48
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object btnUpdate: TSpTBXButton
          Left = 314
          Top = 273
          Width = 77
          Height = 24
          Action = actUpdateItem
          TabOrder = 7
          Images = CommandsDataModule.Images
          ImageIndex = 39
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object lvItems: TTntListView
          Left = 0
          Top = 0
          Width = 394
          Height = 263
          Align = alTop
          Columns = <
            item
              Caption = 'Name'
              Width = 120
            end
            item
              Caption = 'Value'
              Width = 270
            end>
          ColumnClick = False
          GridLines = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnChange = lvItemsChange
        end
      end
    end
    object btnOK: TSpTBXButton
      Left = 149
      Top = 465
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 233
      Top = 465
      Width = 75
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
      Left = 317
      Top = 465
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
    end
  end
  object FormatsPopup: TPopupMenu
    Left = 10
    Top = 460
    object Filename1: TMenuItem
      Caption = 'Filename'
      Hint = 'Placeholder of the Filename'
      OnClick = Filename1Click
    end
    object Linenumber1: TMenuItem
      Tag = 1
      Caption = 'Linenumber'
      Hint = 'Placeholder for the Linenumber'
      OnClick = Filename1Click
    end
    object Columnnumber1: TMenuItem
      Tag = 2
      Caption = 'Columnnumber'
      Hint = 'Placeholder for the Columnnumber'
      OnClick = Filename1Click
    end
  end
  object ActionList: TTntActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 42
    Top = 460
    object actAddItem: TTntAction
      Caption = '&Add'
      Hint = 'Add item'
      ImageIndex = 49
      OnExecute = actAddItemExecute
    end
    object actDeleteItem: TTntAction
      Caption = '&Delete'
      Hint = 'Delete item'
      ImageIndex = 14
      OnExecute = actDeleteItemExecute
    end
    object actMoveUp: TTntAction
      Caption = '&Up'
      Hint = 'Move item up'
      ImageIndex = 47
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TTntAction
      Caption = '&Down'
      Hint = 'Move item down'
      ImageIndex = 48
      OnExecute = actMoveDownExecute
    end
    object actUpdateItem: TTntAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 39
      OnExecute = actUpdateItemExecute
    end
  end
end
