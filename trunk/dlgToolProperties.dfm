object ToolProperties: TToolProperties
  Left = 334
  Top = 153
  HelpContext = 710
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
    object btnOK: TButton
      Left = 149
      Top = 436
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 233
      Top = 436
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object PageControl: TPageControl
      Left = 1
      Top = 1
      Width = 404
      Height = 427
      ActivePage = tsProperties
      Align = alTop
      TabOrder = 2
      object tsProperties: TTabSheet
        Caption = '&Properties'
        object JvGroupBox1: TJvGroupBox
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
          object Label14: TLabel
            Left = 200
            Top = 16
            Width = 48
            Height = 13
            Caption = '(required)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGradientActiveCaption
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
        object JvGroupBox2: TJvGroupBox
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
            Left = 9
            Top = 90
            Width = 252
            Height = 13
            Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGradientActiveCaption
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
            Font.Height = -12
            Font.Name = 'Lucida Console'
            Font.Style = []
            TabOrder = 0
            OnEnter = SynApplicationEnter
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
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
            Font.Height = -12
            Font.Name = 'Lucida Console'
            Font.Style = []
            TabOrder = 1
            OnEnter = SynParametersEnter
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
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
            Font.Height = -12
            Font.Name = 'Lucida Console'
            Font.Style = []
            TabOrder = 2
            OnEnter = SynWorkDirEnter
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.Width = 0
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            WantReturns = False
          end
          object btnAppDir: TButton
            Left = 357
            Top = 15
            Width = 19
            Height = 20
            Caption = '...'
            TabOrder = 3
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
        object JvGroupBox4: TJvGroupBox
          Left = 6
          Top = 187
          Width = 385
          Height = 64
          Caption = 'Shortcut and Context'
          TabOrder = 2
          object Label4: TLabel
            Left = 15
            Top = 18
            Width = 45
            Height = 13
            Caption = '&Shortcut:'
            FocusControl = hkShortCut
          end
          object Label8: TLabel
            Left = 15
            Top = 42
            Width = 43
            Height = 13
            Caption = 'Conte&xt:'
          end
          object Label13: TLabel
            Left = 191
            Top = 18
            Width = 50
            Height = 13
            Caption = 'Sa&ve files:'
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
        object JvGroupBox3: TJvGroupBox
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
          end
          object Label11: TLabel
            Left = 6
            Top = 42
            Width = 61
            Height = 13
            Caption = 'Std. &Output:'
          end
          object Label12: TLabel
            Left = 6
            Top = 70
            Width = 88
            Height = 13
            Caption = 'Messages &Format:'
            FocusControl = edMessagesFormat
          end
          object cbCaptureOutput: TJvCheckBox
            Left = 232
            Top = 13
            Width = 97
            Height = 17
            Caption = '&Capture Output'
            Checked = True
            State = cbChecked
            TabOrder = 2
            LinkedControls = <>
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Shell Dlg 2'
            HotTrackFont.Style = []
          end
          object cbParseMessages: TJvCheckBox
            Left = 232
            Top = 30
            Width = 98
            Height = 17
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Messages'
            Checked = True
            State = cbChecked
            TabOrder = 3
            LinkedControls = <
              item
                Control = edMessagesFormat
              end
              item
                Control = btnStdFormats
              end>
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Shell Dlg 2'
            HotTrackFont.Style = []
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
            TabOrder = 4
          end
          object btnStdFormats: TButton
            Left = 357
            Top = 66
            Width = 19
            Height = 20
            Caption = '<'
            TabOrder = 5
            OnClick = btnStdFormatsClick
          end
          object cbParseTraceback: TJvCheckBox
            Left = 233
            Top = 47
            Width = 100
            Height = 17
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Caption = 'Parse &Traceback'
            Checked = True
            State = cbChecked
            TabOrder = 6
            LinkedControls = <>
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Shell Dlg 2'
            HotTrackFont.Style = []
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
        end
        object JvGroupBox5: TJvGroupBox
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
          object cbHideConsole: TJvCheckBox
            Left = 8
            Top = 16
            Width = 81
            Height = 17
            Hint = 'Check to hide the console.'
            Caption = '&Hide console'
            Checked = True
            State = cbChecked
            TabOrder = 0
            LinkedControls = <>
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Shell Dlg 2'
            HotTrackFont.Style = []
          end
          object cbWaitForTermination: TJvCheckBox
            Left = 104
            Top = 16
            Width = 119
            Height = 17
            Hint = 
              'If checked will monitor the process '#13#10'and notify you when is ter' +
              'minated.'
            Caption = '&Wait for Termination'
            Checked = True
            State = cbChecked
            TabOrder = 1
            LinkedControls = <>
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Shell Dlg 2'
            HotTrackFont.Style = []
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
        object JvGroupBox6: TJvGroupBox
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
        object cbUseCustomEnv: TJvCheckBox
          Left = 123
          Top = 369
          Width = 160
          Height = 17
          Caption = 'Use Customized Environment'
          TabOrder = 2
          LinkedControls = <>
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'MS Shell Dlg 2'
          HotTrackFont.Style = []
        end
        object TBXButton1: TTBXButton
          Left = 4
          Top = 243
          Width = 64
          Height = 24
          Action = actAddItem
          ButtonStyle = bsFlat
          ImageIndex = 49
          Images = CommandsDataModule.Images
          TabOrder = 3
        end
        object TBXButton3: TTBXButton
          Left = 75
          Top = 243
          Width = 64
          Height = 24
          Action = actDeleteItem
          ButtonStyle = bsFlat
          ImageIndex = 14
          Images = CommandsDataModule.Images
          TabOrder = 4
        end
        object TBXButton4: TTBXButton
          Left = 146
          Top = 243
          Width = 80
          Height = 24
          Action = actMoveUp
          ButtonStyle = bsFlat
          ImageIndex = 47
          Images = CommandsDataModule.Images
          TabOrder = 5
        end
        object TBXButton5: TTBXButton
          Left = 233
          Top = 243
          Width = 84
          Height = 24
          Action = actMoveDown
          ButtonStyle = bsFlat
          ImageIndex = 48
          Images = CommandsDataModule.Images
          TabOrder = 6
        end
        object TBXButton2: TTBXButton
          Left = 324
          Top = 243
          Width = 66
          Height = 24
          Action = actUpdateItem
          ButtonStyle = bsFlat
          ImageIndex = 39
          Images = CommandsDataModule.Images
          TabOrder = 7
        end
      end
    end
    object btnHelp: TButton
      Left = 317
      Top = 436
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 3
      OnClick = btnHelpClick
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
