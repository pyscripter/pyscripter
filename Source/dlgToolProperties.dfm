inherited ToolProperties: TToolProperties
  Left = 334
  Top = 153
  HelpContext = 710
  Caption = 'External Tool Properties'
  ClientHeight = 498
  ClientWidth = 406
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 412
  ExplicitHeight = 527
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 406
    Height = 498
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    DesignSize = (
      406
      498)
    object btnOK: TButton
      Left = 149
      Top = 460
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 233
      Top = 460
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 317
      Top = 460
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object TabControl: TSpTBXTabControl
      Left = 1
      Top = 1
      Width = 404
      Height = 453
      Align = alTop
      ActiveTabIndex = 0
      ExplicitLeft = 2
      HiddenItems = <>
      object SpTBXTabItem1: TSpTBXTabItem
        Caption = '&Properties'
        Checked = True
      end
      object SpTBXTabItem2: TSpTBXTabItem
        Caption = '&Environment'
      end
      object tabEnvironment: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 404
        Height = 428
        Caption = '&Environment'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem2'
        object GroupBox6: TGroupBox
          Left = 7
          Top = 307
          Width = 385
          Height = 83
          Caption = 'Name-Value Pair'
          TabOrder = 1
          object Label15: TLabel
            Left = 14
            Top = 28
            Width = 41
            Height = 13
            Caption = '&Name:'
            FocusControl = edEnvName
          end
          object Label16: TLabel
            Left = 14
            Top = 50
            Width = 41
            Height = 13
            Caption = '&Value:'
          end
          object edEnvName: TEdit
            Left = 77
            Top = 23
            Width = 130
            Height = 21
            TabOrder = 0
          end
          object edEnvValue: TEdit
            Left = 77
            Top = 48
            Width = 293
            Height = 21
            Cursor = crIBeam
            HideSelection = False
            TabOrder = 1
          end
        end
        object cbUseCustomEnv: TCheckBox
          Left = 123
          Top = 401
          Width = 163
          Height = 21
          Caption = 'Use Customized Environment'
          TabOrder = 2
        end
        object btnAdd: TButton
          Left = 61
          Top = 269
          Width = 77
          Height = 24
          Action = actAddItem
          Images = CommandsDataModule.Images
          TabOrder = 3
        end
        object btnDelete: TButton
          Left = 163
          Top = 269
          Width = 77
          Height = 24
          Action = actDeleteItem
          Images = CommandsDataModule.Images
          TabOrder = 4
        end
        object btnUpdate: TButton
          Left = 265
          Top = 269
          Width = 77
          Height = 24
          Action = actUpdateItem
          Images = CommandsDataModule.Images
          TabOrder = 5
        end
        object lvItems: TListView
          Left = 2
          Top = 0
          Width = 398
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
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = lvItemsSelectItem
        end
      end
      object tabProperties: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 404
        Height = 428
        Caption = '&Properties'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem1'
        object GroupBox1: TGroupBox
          Left = 7
          Top = 5
          Width = 384
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
            Left = 212
            Top = 16
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
          object edName: TEdit
            Left = 77
            Top = 13
            Width = 124
            Height = 21
            TabOrder = 0
          end
          object edDescription: TEdit
            Left = 77
            Top = 36
            Width = 301
            Height = 21
            TabOrder = 1
          end
        end
        object GroupBox2: TGroupBox
          Left = 7
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
            Left = 8
            Top = 86
            Width = 252
            Height = 13
            Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
            Enabled = False
          end
          object SynApplication: TSynEdit
            Left = 77
            Top = 18
            Width = 277
            Height = 18
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 0
            OnEnter = SynApplicationEnter
            CodeFolding.GutterShapeSize = 11
            CodeFolding.CollapsedLineColor = clGrayText
            CodeFolding.FolderBarLinesColor = clGrayText
            CodeFolding.IndentGuidesColor = clGray
            CodeFolding.IndentGuides = True
            CodeFolding.ShowCollapsedLine = False
            CodeFolding.ShowHintMark = True
            UseCodeFolding = False
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
            FontSmoothing = fsmNone
          end
          object SynParameters: TSynEdit
            Left = 77
            Top = 40
            Width = 296
            Height = 18
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 2
            OnEnter = SynParametersEnter
            CodeFolding.GutterShapeSize = 11
            CodeFolding.CollapsedLineColor = clGrayText
            CodeFolding.FolderBarLinesColor = clGrayText
            CodeFolding.IndentGuidesColor = clGray
            CodeFolding.IndentGuides = True
            CodeFolding.ShowCollapsedLine = False
            CodeFolding.ShowHintMark = True
            UseCodeFolding = False
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
            FontSmoothing = fsmNone
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
            CodeFolding.GutterShapeSize = 11
            CodeFolding.CollapsedLineColor = clGrayText
            CodeFolding.FolderBarLinesColor = clGrayText
            CodeFolding.IndentGuidesColor = clGray
            CodeFolding.IndentGuides = True
            CodeFolding.ShowCollapsedLine = False
            CodeFolding.ShowHintMark = True
            UseCodeFolding = False
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
            FontSmoothing = fsmNone
          end
          object btnAppDir: TButton
            Left = 357
            Top = 15
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 1
            OnClick = btnAppDirClick
          end
          object btnWorkDir: TButton
            Left = 357
            Top = 63
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 4
            OnClick = btnWorkDirClick
          end
        end
        object GroupBox4: TGroupBox
          Left = 7
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
            Left = 232
            Top = 18
            Width = 50
            Height = 13
            Caption = 'Sa&ve files:'
            FocusControl = cbSaveFiles
          end
          object cbContext: TComboBox
            Left = 86
            Top = 36
            Width = 125
            Height = 21
            Hint = 'Specifies the context in which'#13#10'the tool is enabled.'
            Style = csDropDownList
            TabOrder = 0
            Items.Strings = (
              'Always enabled'
              'Active editor'
              'Active Python file'
              'Selection available')
          end
          object cbSaveFiles: TComboBox
            Left = 233
            Top = 37
            Width = 142
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            Items.Strings = (
              'None'
              'Active only'
              'All files')
          end
        end
        object GroupBox3: TGroupBox
          Left = 3
          Top = 257
          Width = 383
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
          object btnStdFormats: TButton
            Left = 357
            Top = 66
            Width = 19
            Height = 20
            Caption = '<'
            DropDownMenu = FormatsPopup
            TabOrder = 6
            OnClick = btnStdFormatsClick
          end
          object cbCaptureOutput: TCheckBox
            Left = 233
            Top = 13
            Width = 141
            Height = 21
            Caption = '&Capture Output'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object cbParseMessages: TCheckBox
            Left = 233
            Top = 30
            Width = 141
            Height = 21
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Messages'
            Checked = True
            State = cbChecked
            TabOrder = 3
            OnClick = cbParseMessagesClick
          end
          object cbParseTraceback: TCheckBox
            Left = 233
            Top = 47
            Width = 141
            Height = 21
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Traceback'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object edMessagesFormat: TEdit
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
          end
          object cbStandardInput: TComboBox
            Left = 86
            Top = 16
            Width = 125
            Height = 21
            Hint = 
              'Select option other than None to '#13#10'feed the corresponding inform' +
              'ation'#13#10'to the standard input.'
            Style = csDropDownList
            TabOrder = 0
            Items.Strings = (
              'None'
              'Word at cursor'
              'Current line'
              'Selection'
              'ActiveFile')
          end
          object cbStandardOutput: TComboBox
            Left = 86
            Top = 40
            Width = 125
            Height = 21
            Hint = 
              'Select option other than None to '#13#10'redirect the output to the '#13#10 +
              'corresponding outlet.'
            Style = csDropDownList
            TabOrder = 1
            Items.Strings = (
              'None'
              'Word at cursor'
              'Current line'
              'Selection'
              'Active file'
              'New file')
          end
        end
        object GroupBox5: TGroupBox
          Left = 7
          Top = 357
          Width = 384
          Height = 65
          Caption = 'Console'
          TabOrder = 4
          object Label9: TLabel
            Left = 240
            Top = 18
            Width = 130
            Height = 13
            Caption = '&Timeout (ms):'
            FocusControl = seTimeout
          end
          object cbHideConsole: TCheckBox
            Left = 8
            Top = 16
            Width = 203
            Height = 21
            Hint = 'Check to hide the console.'
            Caption = '&Hide console'
            Checked = True
            State = cbChecked
            TabOrder = 0
          end
          object cbWaitForTermination: TCheckBox
            Left = 8
            Top = 37
            Width = 208
            Height = 21
            Hint = 
              'If checked will monitor the process '#13#10'and notify you when is ter' +
              'minated.'
            Caption = '&Wait for Termination'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object seTimeout: TSpinEdit
            Left = 240
            Top = 37
            Width = 79
            Height = 22
            Hint = 
              'If set to a value <> 0 then you will be prompted '#13#10'to abort the ' +
              'program after the specified time (ms).'
            Increment = 100
            MaxValue = 10000
            MinValue = 0
            TabOrder = 2
            Value = 0
          end
        end
      end
    end
  end
  object FormatsPopup: TPopupMenu
    Left = 290
    Top = 4
    object Filename1: TMenuItem
      Caption = 'Filename'
      Hint = 'Placeholder of the Filename'
      OnClick = Filename1Click
    end
    object Linenumber1: TMenuItem
      Tag = 1
      Caption = 'Line number'
      Hint = 'Placeholder for the line number'
      OnClick = Filename1Click
    end
    object Columnnumber1: TMenuItem
      Tag = 2
      Caption = 'Column number'
      Hint = 'Placeholder for the column number'
      OnClick = Filename1Click
    end
  end
  object ActionList: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 354
    Top = 4
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
    object actUpdateItem: TAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 39
      OnExecute = actUpdateItemExecute
    end
  end
end
