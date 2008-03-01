object ToolProperties: TToolProperties
  Left = 334
  Top = 153
  HelpContext = 710
  ActiveControl = seTimeout
  BorderStyle = bsDialog
  Caption = 'External Tool Properties'
  ClientHeight = 469
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 406
    Height = 469
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    DesignSize = (
      406
      469)
    object PageControl: TPageControl
      Left = 1
      Top = 1
      Width = 404
      Height = 427
      ActivePage = tsProperties
      Align = alTop
      TabOrder = 0
      object tsProperties: TTabSheet
        Caption = '&Properties'
        object GroupBox1: TGroupBox
          Left = 4
          Top = 5
          Width = 385
          Height = 65
          Caption = 'General'
          TabOrder = 0
          object Label1: TLabel
            Left = 8
            Top = 16
            Width = 31
            Height = 13
            Caption = '&Name:'
            FocusControl = edName
          end
          object Label5: TLabel
            Left = 8
            Top = 39
            Width = 57
            Height = 13
            Caption = 'Desc&ription:'
            FocusControl = edDescription
          end
          object Label17: TLabel
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
            ParentFont = False
          end
          object edDescription: TEdit
            Left = 70
            Top = 36
            Width = 308
            Height = 21
            TabOrder = 1
          end
          object edName: TEdit
            Left = 70
            Top = 13
            Width = 119
            Height = 21
            TabOrder = 0
          end
        end
        object GroupBox2: TGroupBox
          Left = 4
          Top = 73
          Width = 385
          Height = 108
          Caption = 'Command'
          TabOrder = 1
          object Label2: TLabel
            Left = 6
            Top = 18
            Width = 56
            Height = 13
            Caption = '&Application:'
            FocusControl = SynApplication
          end
          object Label6: TLabel
            Left = 6
            Top = 42
            Width = 59
            Height = 13
            Caption = '&Parameters:'
            FocusControl = SynParameters
          end
          object Label7: TLabel
            Left = 6
            Top = 66
            Width = 89
            Height = 13
            Caption = 'Working &directory:'
            FocusControl = SynWorkDir
          end
          object Label3: TLabel
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
            ParentFont = False
          end
          object SynApplication: TSynEdit
            Left = 64
            Top = 16
            Width = 289
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
            Left = 64
            Top = 40
            Width = 309
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
            Left = 96
            Top = 64
            Width = 257
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
            LinkFont.Name = 'Tahoma'
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
            LinkFont.Name = 'Tahoma'
            LinkFont.Style = [fsUnderline]
          end
        end
        object GroupBox4: TGroupBox
          Left = 6
          Top = 187
          Width = 385
          Height = 64
          Caption = 'Shortcut and Context'
          TabOrder = 2
          object lbShortcut: TLabel
            Left = 11
            Top = 18
            Width = 45
            Height = 13
            Caption = '&Shortcut:'
            FocusControl = hkShortCut
          end
          object lbContext: TLabel
            Left = 13
            Top = 40
            Width = 43
            Height = 13
            Caption = 'Conte&xt:'
            FocusControl = cbContext
          end
          object Label13: TLabel
            Left = 191
            Top = 18
            Width = 50
            Height = 13
            Caption = 'Sa&ve files:'
            FocusControl = cbSaveFiles
          end
          object hkShortCut: TJvHotKey
            Left = 61
            Top = 15
            Width = 107
            Height = 19
            Hint = 'Allows you to specify a menu shortcut for the tool'
            HotKey = 0
            InvalidKeys = [hcNone]
            Modifiers = []
            TabOrder = 0
            ParentColor = False
          end
          object cbContext: TComboBox
            Left = 62
            Top = 37
            Width = 124
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
          end
          object cbSaveFiles: TComboBox
            Left = 245
            Top = 15
            Width = 124
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 2
            Text = 'None'
            Items.Strings = (
              'None'
              'Active only'
              'All files')
          end
        end
        object GroupBox3: TGroupBox
          Left = 5
          Top = 258
          Width = 385
          Height = 94
          Caption = 'Standard Input and Output'
          TabOrder = 3
          object Label10: TLabel
            Left = 6
            Top = 18
            Width = 53
            Height = 13
            Caption = 'Std. &Input:'
            FocusControl = cbStandardInput
          end
          object Label11: TLabel
            Left = 6
            Top = 42
            Width = 61
            Height = 13
            Caption = 'Std. &Output:'
            FocusControl = cbStandardOutput
          end
          object Label12: TLabel
            Left = 6
            Top = 70
            Width = 88
            Height = 13
            Caption = 'Messages &Format:'
            FocusControl = edMessagesFormat
          end
          object Label14: TLabel
            Left = 160
            Top = 40
            Width = 37
            Height = 13
            Caption = 'Label14'
          end
          object edMessagesFormat: TEdit
            Left = 98
            Top = 66
            Width = 255
            Height = 21
            Hint = 
              'A grep expression for parsing output lines.'#13#10'Use the button on t' +
              'he right to insert '#13#10'predefined grep expressions.'
            AutoSelect = False
            AutoSize = False
            TabOrder = 5
          end
          object cbStandardInput: TComboBox
            Left = 68
            Top = 16
            Width = 137
            Height = 21
            Hint = 
              'Select option other than None to '#13#10'feed the corresponding inform' +
              'ation'#13#10'to the standard input.'
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 0
            Text = 'None'
            Items.Strings = (
              'None'
              'Word at cursor'
              'Current line'
              'Selection'
              'ActiveFile')
          end
          object cbStandardOutput: TComboBox
            Left = 68
            Top = 40
            Width = 137
            Height = 21
            Hint = 
              'Select option other than None to '#13#10'redirect the output to the '#13#10 +
              'corresponding outlet.'
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 1
            Text = 'None'
            Items.Strings = (
              'None'
              'Word at cursor'
              'Current line'
              'Selection'
              'Active file'
              'New file')
          end
          object btnStdFormats: TSpTBXButton
            Left = 357
            Top = 66
            Width = 19
            Height = 20
            Caption = '<'
            TabOrder = 6
            OnClick = btnStdFormatsClick
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'Tahoma'
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
        end
        object GroupBox5: TGroupBox
          Left = 4
          Top = 353
          Width = 385
          Height = 41
          Caption = 'Console'
          TabOrder = 4
          object Label9: TLabel
            Left = 239
            Top = 18
            Width = 66
            Height = 13
            Caption = '&Timeout (ms):'
            FocusControl = seTimeout
          end
          object seTimeout: TJvSpinEdit
            Left = 312
            Top = 16
            Width = 65
            Height = 21
            Hint = 
              'If set to a value <> 0 then you will be prompted '#13#10'to abort the ' +
              'program after the specified time.'
            CheckMinValue = True
            Increment = 100.000000000000000000
            TabOrder = 2
          end
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
            Left = 104
            Top = 16
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
        end
      end
      object tsEnvironment: TTabSheet
        Caption = '&Environment'
        ImageIndex = 1
        object lvItems: TListView
          Left = 0
          Top = 0
          Width = 396
          Height = 234
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
        object GroupBox6: TGroupBox
          Left = 3
          Top = 275
          Width = 389
          Height = 83
          Caption = 'Name-Value Pair'
          TabOrder = 1
          object Label15: TLabel
            Left = 14
            Top = 28
            Width = 31
            Height = 13
            Caption = '&Name:'
            FocusControl = edEnvName
          end
          object Label16: TLabel
            Left = 14
            Top = 50
            Width = 30
            Height = 13
            Caption = '&Value:'
          end
          object edEnvName: TEdit
            Left = 61
            Top = 23
            Width = 121
            Height = 21
            TabOrder = 0
          end
          object edEnvValue: TEdit
            Left = 62
            Top = 48
            Width = 308
            Height = 21
            Cursor = crIBeam
            HideSelection = False
            TabOrder = 1
          end
        end
        object cbUseCustomEnv: TSpTBXCheckBox
          Left = 123
          Top = 369
          Width = 157
          Height = 15
          Caption = 'Use Customized Environment'
          ParentColor = True
          TabOrder = 2
          ThemeType = thtWindows
        end
        object TBXButton1: TSpTBXButton
          Left = 4
          Top = 243
          Width = 64
          Height = 24
          Action = actAddItem
          TabOrder = 3
          Images = CommandsDataModule.Images
          ImageIndex = 49
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'Tahoma'
          LinkFont.Style = [fsUnderline]
        end
        object TBXButton3: TSpTBXButton
          Left = 75
          Top = 243
          Width = 64
          Height = 24
          Action = actDeleteItem
          TabOrder = 4
          Images = CommandsDataModule.Images
          ImageIndex = 14
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'Tahoma'
          LinkFont.Style = [fsUnderline]
        end
        object TBXButton4: TSpTBXButton
          Left = 146
          Top = 243
          Width = 80
          Height = 24
          Action = actMoveUp
          TabOrder = 5
          Images = CommandsDataModule.Images
          ImageIndex = 47
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'Tahoma'
          LinkFont.Style = [fsUnderline]
        end
        object TBXButton5: TSpTBXButton
          Left = 233
          Top = 243
          Width = 84
          Height = 24
          Action = actMoveDown
          TabOrder = 6
          Images = CommandsDataModule.Images
          ImageIndex = 48
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'Tahoma'
          LinkFont.Style = [fsUnderline]
        end
        object TBXButton2: TSpTBXButton
          Left = 324
          Top = 243
          Width = 66
          Height = 24
          Action = actUpdateItem
          TabOrder = 7
          Images = CommandsDataModule.Images
          ImageIndex = 39
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'Tahoma'
          LinkFont.Style = [fsUnderline]
        end
      end
    end
    object btnOK: TSpTBXButton
      Left = 149
      Top = 436
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Default = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 233
      Top = 436
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Cancel = True
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 317
      Top = 436
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
  end
  object FormatsPopup: TPopupMenu
    Left = 10
    Top = 438
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
  object ActionList: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 46
    Top = 436
    object actAddItem: TAction
      Caption = '&Add'
      Hint = 'Add item'
      ImageIndex = 49
      OnExecute = actAddItemExecute
    end
    object actDeleteItem: TAction
      Caption = '&Delete'
      Hint = 'Delete item'
      ImageIndex = 14
      OnExecute = actDeleteItemExecute
    end
    object actMoveUp: TAction
      Caption = 'Move &Up'
      Hint = 'Move item up'
      ImageIndex = 47
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = 'Move &Down'
      Hint = 'Move item down'
      ImageIndex = 48
      OnExecute = actMoveDownExecute
    end
    object actUpdateItem: TAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 39
      OnExecute = actUpdateItemExecute
    end
  end
end
