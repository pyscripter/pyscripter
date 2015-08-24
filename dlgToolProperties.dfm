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
  object Panel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 406
    Height = 498
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Borders = False
    TBXStyleBackground = True
    DesignSize = (
      406
      498)
    object btnOK: TSpTBXButton
      Left = 149
      Top = 465
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 0
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 233
      Top = 465
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Cancel = True
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 317
      Top = 465
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object TabControl: TSpTBXTabControl
      Left = 0
      Top = 0
      Width = 406
      Height = 453
      Align = alTop
      ActiveTabIndex = 0
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
        Width = 406
        Height = 428
        Caption = '&Environment'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem2'
        object GroupBox6: TSpTBXGroupBox
          Left = 7
          Top = 307
          Width = 385
          Height = 83
          Caption = 'Name-Value Pair'
          TabOrder = 1
          TBXStyleBackground = True
          object Label15: TSpTBXLabel
            Left = 14
            Top = 28
            Width = 37
            Height = 19
            Caption = '&Name:'
            FocusControl = edEnvName
          end
          object Label16: TSpTBXLabel
            Left = 14
            Top = 50
            Width = 36
            Height = 19
            Caption = '&Value:'
          end
          object edEnvName: TSpTBXEdit
            Left = 61
            Top = 23
            Width = 121
            Height = 21
            TabOrder = 0
          end
          object edEnvValue: TSpTBXEdit
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
          Top = 401
          Width = 163
          Height = 21
          Caption = 'Use Customized Environment'
          TabOrder = 2
        end
        object btnAdd: TSpTBXButton
          Left = 3
          Top = 270
          Width = 77
          Height = 24
          Action = actAddItem
          TabOrder = 3
          Images = CommandsDataModule.Images
          ImageIndex = 49
        end
        object btnDelete: TSpTBXButton
          Left = 81
          Top = 270
          Width = 77
          Height = 24
          Action = actDeleteItem
          TabOrder = 4
          Images = CommandsDataModule.Images
          ImageIndex = 14
        end
        object btnMoveUp: TSpTBXButton
          Left = 160
          Top = 270
          Width = 77
          Height = 24
          Action = actMoveUp
          TabOrder = 5
          Images = CommandsDataModule.Images
          ImageIndex = 47
        end
        object btnMoveDown: TSpTBXButton
          Left = 239
          Top = 270
          Width = 77
          Height = 24
          Action = actMoveDown
          TabOrder = 6
          Images = CommandsDataModule.Images
          ImageIndex = 48
        end
        object btnUpdate: TSpTBXButton
          Left = 318
          Top = 270
          Width = 77
          Height = 24
          Action = actUpdateItem
          TabOrder = 7
          Images = CommandsDataModule.Images
          ImageIndex = 39
        end
        object lvItems: TEasyListview
          AlignWithMargins = True
          Left = 5
          Top = 3
          Width = 394
          Height = 263
          Align = alTop
          EditManager.Font.Charset = DEFAULT_CHARSET
          EditManager.Font.Color = clWindowText
          EditManager.Font.Height = -11
          EditManager.Font.Name = 'Tahoma'
          EditManager.Font.Style = []
          Header.Columns.Items = {
            0600000002000000110000005445617379436F6C756D6E53746F726564FFFECE
            0006000000800800010100010000000000000178000000FFFFFF1F0001000000
            01000000040000004E0061006D00650000000000000000000000000011000000
            5445617379436F6C756D6E53746F726564FFFECE000600000080080001010001
            010000000000000E010000FFFFFF1F0001000000010000000500000056006100
            6C0075006500000000000000000000000000}
          Header.Draggable = False
          Header.Visible = True
          HotTrack.Enabled = True
          PaintInfoGroup.MarginBottom.CaptionIndent = 4
          PaintInfoItem.GridLines = True
          TabOrder = 0
          View = elsReport
          OnColumnClick = lvItemsColumnClick
          OnItemSelectionsChanged = lvItemsItemSelectionsChanged
          ExplicitLeft = 3
        end
      end
      object tabProperties: TSpTBXTabSheet
        Left = 0
        Top = 25
        Width = 406
        Height = 428
        Caption = '&Properties'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem1'
        object GroupBox1: TSpTBXGroupBox
          Left = 7
          Top = 5
          Width = 384
          Height = 65
          Caption = 'General'
          TabOrder = 0
          TBXStyleBackground = True
          object Label1: TSpTBXLabel
            Left = 8
            Top = 16
            Width = 37
            Height = 19
            Caption = '&Name:'
            FocusControl = edName
          end
          object Label5: TSpTBXLabel
            Left = 8
            Top = 39
            Width = 63
            Height = 19
            Caption = 'Desc&ription:'
            FocusControl = edDescription
          end
          object Label17: TSpTBXLabel
            Left = 212
            Top = 16
            Width = 54
            Height = 19
            Caption = '(required)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = []
            ParentFont = False
          end
          object edName: TSpTBXEdit
            Left = 77
            Top = 13
            Width = 124
            Height = 21
            TabOrder = 0
          end
          object edDescription: TSpTBXEdit
            Left = 77
            Top = 36
            Width = 301
            Height = 21
            TabOrder = 1
          end
        end
        object GroupBox2: TSpTBXGroupBox
          Left = 7
          Top = 73
          Width = 385
          Height = 108
          Caption = 'Command'
          TabOrder = 1
          TBXStyleBackground = True
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
          object btnAppDir: TSpTBXButton
            Left = 357
            Top = 15
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 1
            OnClick = btnAppDirClick
          end
          object btnWorkDir: TSpTBXButton
            Left = 357
            Top = 63
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 4
            OnClick = btnWorkDirClick
          end
          object Label2: TSpTBXLabel
            Left = 6
            Top = 18
            Width = 62
            Height = 19
            Caption = '&Application:'
            FocusControl = SynApplication
          end
          object Label6: TSpTBXLabel
            Left = 6
            Top = 42
            Width = 65
            Height = 19
            Caption = '&Parameters:'
            FocusControl = SynParameters
          end
          object Label7: TSpTBXLabel
            Left = 6
            Top = 66
            Width = 95
            Height = 19
            Caption = 'Working &directory:'
            FocusControl = SynWorkDir
          end
          object Label3: TSpTBXLabel
            Left = 8
            Top = 86
            Width = 258
            Height = 19
            Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
            Enabled = False
          end
        end
        object GroupBox4: TSpTBXGroupBox
          Left = 7
          Top = 187
          Width = 385
          Height = 64
          Caption = 'Shortcut and Context'
          TabOrder = 2
          TBXStyleBackground = True
          object lbShortcut: TSpTBXLabel
            Left = 11
            Top = 18
            Width = 51
            Height = 19
            Caption = '&Shortcut:'
            FocusControl = hkShortCut
          end
          object lbContext: TSpTBXLabel
            Left = 13
            Top = 40
            Width = 49
            Height = 19
            Caption = 'Conte&xt:'
            FocusControl = cbContext
          end
          object Label13: TSpTBXLabel
            Left = 232
            Top = 18
            Width = 56
            Height = 19
            Caption = 'Sa&ve files:'
            FocusControl = cbSaveFiles
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
          end
        end
        object GroupBox3: TSpTBXGroupBox
          Left = 7
          Top = 258
          Width = 383
          Height = 94
          Caption = 'Standard Input and Output'
          TabOrder = 3
          TBXStyleBackground = True
          object btnStdFormats: TSpTBXButton
            Left = 357
            Top = 66
            Width = 19
            Height = 20
            Caption = '<'
            TabOrder = 6
            DropDownArrow = False
            DropDownMenu = FormatsPopup
          end
          object cbCaptureOutput: TSpTBXCheckBox
            Left = 233
            Top = 13
            Width = 100
            Height = 21
            Caption = '&Capture Output'
            ParentColor = True
            TabOrder = 2
            Checked = True
            State = cbChecked
          end
          object cbParseMessages: TSpTBXCheckBox
            Left = 233
            Top = 30
            Width = 101
            Height = 21
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Messages'
            ParentColor = True
            TabOrder = 3
            OnClick = cbParseMessagesClick
            Checked = True
            State = cbChecked
          end
          object cbParseTraceback: TSpTBXCheckBox
            Left = 233
            Top = 47
            Width = 103
            Height = 21
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Traceback'
            ParentColor = True
            TabOrder = 4
            Checked = True
            State = cbChecked
          end
          object Label10: TSpTBXLabel
            Left = 6
            Top = 18
            Width = 59
            Height = 19
            Caption = 'Std. &Input:'
            FocusControl = cbStandardInput
          end
          object Label11: TSpTBXLabel
            Left = 6
            Top = 42
            Width = 67
            Height = 19
            Caption = 'Std. &Output:'
            FocusControl = cbStandardOutput
          end
          object Label12: TSpTBXLabel
            Left = 6
            Top = 70
            Width = 94
            Height = 19
            Caption = 'Messages &Format:'
            FocusControl = edMessagesFormat
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
          end
        end
        object GroupBox5: TSpTBXGroupBox
          Left = 7
          Top = 357
          Width = 384
          Height = 65
          Caption = 'Console'
          TabOrder = 4
          TBXStyleBackground = True
          object cbHideConsole: TSpTBXCheckBox
            Left = 8
            Top = 16
            Width = 84
            Height = 21
            Hint = 'Check to hide the console.'
            Caption = '&Hide console'
            ParentColor = True
            TabOrder = 0
            Checked = True
            State = cbChecked
          end
          object cbWaitForTermination: TSpTBXCheckBox
            Left = 8
            Top = 37
            Width = 122
            Height = 21
            Hint = 
              'If checked will monitor the process '#13#10'and notify you when is ter' +
              'minated.'
            Caption = '&Wait for Termination'
            ParentColor = True
            TabOrder = 1
            Checked = True
            State = cbChecked
          end
          object Label9: TSpTBXLabel
            Left = 240
            Top = 18
            Width = 72
            Height = 19
            Caption = '&Timeout (ms):'
            FocusControl = seTimeout
          end
          object seTimeout: TSpTBXSpinEdit
            Left = 240
            Top = 37
            Width = 79
            Height = 21
            Hint = 
              'If set to a value <> 0 then you will be prompted '#13#10'to abort the ' +
              'program after the specified time.'
            Alignment = taLeftJustify
            TabOrder = 2
            SpinButton.Left = 60
            SpinButton.Top = 0
            SpinButton.Width = 15
            SpinButton.Height = 17
            SpinButton.Align = alRight
            SpinOptions.Increment = 100.000000000000000000
          end
        end
      end
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
  object ActionList: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 42
    Top = 460
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
      Caption = '&Up'
      Hint = 'Move item up'
      ImageIndex = 47
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = '&Down'
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
