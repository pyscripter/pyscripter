inherited ToolProperties: TToolProperties
  Left = 334
  Top = 153
  HelpContext = 710
  Caption = 'External Tool Properties'
  ClientHeight = 498
  ClientWidth = 444
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 444
    Height = 498
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    DesignSize = (
      444
      498)
    object btnOK: TButton
      Left = 187
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
      Left = 271
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
      Left = 355
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
      Width = 442
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
        Width = 442
        Height = 428
        Caption = '&Environment'
        ImageIndex = -1
        TabItem = 'SpTBXTabItem2'
        object GroupBox6: TGroupBox
          Left = 7
          Top = 307
          Width = 426
          Height = 83
          Caption = 'Name-Value Pair'
          TabOrder = 1
          DesignSize = (
            426
            83)
          object Label15: TLabel
            Left = 14
            Top = 28
            Width = 35
            Height = 15
            Caption = '&Name:'
            FocusControl = edEnvName
          end
          object Label16: TLabel
            Left = 14
            Top = 50
            Width = 31
            Height = 15
            Caption = '&Value:'
          end
          object edEnvName: TEdit
            Left = 77
            Top = 23
            Width = 171
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edEnvValue: TEdit
            Left = 77
            Top = 48
            Width = 334
            Height = 23
            Cursor = crIBeam
            Anchors = [akLeft, akTop, akRight]
            HideSelection = False
            TabOrder = 1
          end
        end
        object cbUseCustomEnv: TCheckBox
          Left = 21
          Top = 396
          Width = 356
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
          Images = vilImages
          TabOrder = 3
        end
        object btnDelete: TButton
          Left = 163
          Top = 269
          Width = 77
          Height = 24
          Action = actDeleteItem
          Images = vilImages
          TabOrder = 4
        end
        object btnUpdate: TButton
          Left = 265
          Top = 269
          Width = 77
          Height = 24
          Action = actUpdateItem
          Images = vilImages
          TabOrder = 5
        end
        object lvItems: TListView
          Left = 2
          Top = 0
          Width = 436
          Height = 263
          Align = alTop
          Columns = <
            item
              Caption = 'Name'
              Width = 160
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
        Width = 442
        Height = 428
        Caption = '&Properties'
        ImageIndex = -1
        DesignSize = (
          442
          428)
        TabItem = 'SpTBXTabItem1'
        object GroupBox1: TGroupBox
          Left = 7
          Top = 5
          Width = 428
          Height = 70
          Anchors = [akLeft, akTop, akRight]
          Caption = 'General'
          TabOrder = 0
          DesignSize = (
            428
            70)
          object Label1: TLabel
            Left = 6
            Top = 16
            Width = 35
            Height = 15
            Caption = '&Name:'
            FocusControl = edName
          end
          object Label5: TLabel
            Left = 6
            Top = 43
            Width = 63
            Height = 15
            Caption = 'Desc&ription:'
            FocusControl = edDescription
          end
          object Label17: TLabel
            Left = 301
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
            Left = 90
            Top = 13
            Width = 193
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edDescription: TEdit
            Left = 90
            Top = 40
            Width = 325
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
        end
        object GroupBox2: TGroupBox
          Left = 6
          Top = 79
          Width = 428
          Height = 108
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Command'
          TabOrder = 1
          DesignSize = (
            428
            108)
          object Label2: TLabel
            Left = 6
            Top = 18
            Width = 64
            Height = 15
            Caption = '&Application:'
            FocusControl = SynApplication
          end
          object Label6: TLabel
            Left = 6
            Top = 43
            Width = 62
            Height = 15
            Caption = '&Parameters:'
            FocusControl = SynParameters
          end
          object Label7: TLabel
            Left = 6
            Top = 66
            Width = 98
            Height = 15
            Caption = 'Working &directory:'
            FocusControl = SynWorkDir
          end
          object Label3: TLabel
            Left = 8
            Top = 86
            Width = 272
            Height = 15
            Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
            Enabled = False
          end
          object SynApplication: TSynEdit
            Left = 90
            Top = 18
            Width = 303
            Height = 18
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            Font.Quality = fqClearTypeNatural
            TabOrder = 0
            OnEnter = SynEditEnter
            UseCodeFolding = False
            ExtraLineSpacing = 0
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
            HideSelection = True
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            SelectedColor.Alpha = 0.400000005960464500
            WantReturns = False
          end
          object SynParameters: TSynEdit
            Left = 90
            Top = 41
            Width = 325
            Height = 18
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            Font.Quality = fqClearTypeNatural
            TabOrder = 2
            OnEnter = SynEditEnter
            UseCodeFolding = False
            ExtraLineSpacing = 0
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
            HideSelection = True
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            SelectedColor.Alpha = 0.400000005960464500
            WantReturns = False
          end
          object SynWorkDir: TSynEdit
            Left = 114
            Top = 64
            Width = 275
            Height = 18
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            Font.Quality = fqClearTypeNatural
            TabOrder = 3
            OnEnter = SynEditEnter
            UseCodeFolding = False
            ExtraLineSpacing = 0
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
            HideSelection = True
            Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
            ScrollBars = ssNone
            SelectedColor.Alpha = 0.400000005960464500
            WantReturns = False
          end
          object btnAppDir: TButton
            Left = 395
            Top = 15
            Width = 19
            Height = 20
            Anchors = [akTop, akRight]
            Caption = '...'
            TabOrder = 1
            OnClick = btnAppDirClick
          end
          object btnWorkDir: TButton
            Left = 395
            Top = 63
            Width = 19
            Height = 20
            Anchors = [akTop, akRight]
            Caption = '...'
            TabOrder = 4
            OnClick = btnWorkDirClick
          end
        end
        object GroupBox4: TGroupBox
          Left = 6
          Top = 193
          Width = 428
          Height = 64
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shortcut and Context'
          TabOrder = 2
          DesignSize = (
            428
            64)
          object lbShortcut: TLabel
            Left = 6
            Top = 18
            Width = 48
            Height = 15
            Caption = '&Shortcut:'
          end
          object lbContext: TLabel
            Left = 6
            Top = 40
            Width = 45
            Height = 15
            Caption = 'Conte&xt:'
            FocusControl = cbContext
          end
          object Label13: TLabel
            Left = 270
            Top = 18
            Width = 51
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'Sa&ve files:'
            FocusControl = cbSaveFiles
          end
          object cbContext: TComboBox
            Left = 90
            Top = 36
            Width = 140
            Height = 23
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
            Left = 271
            Top = 37
            Width = 142
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            Items.Strings = (
              'None'
              'Active only'
              'All files')
          end
        end
        object GroupBox3: TGroupBox
          Left = 6
          Top = 260
          Width = 428
          Height = 115
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Standard Input and Output'
          TabOrder = 3
          DesignSize = (
            428
            115)
          object Label10: TLabel
            Left = 6
            Top = 26
            Width = 54
            Height = 15
            Caption = 'Std. &Input:'
            FocusControl = cbStandardInput
          end
          object Label11: TLabel
            Left = 6
            Top = 54
            Width = 64
            Height = 15
            Caption = 'Std. &Output:'
            FocusControl = cbStandardOutput
          end
          object Label12: TLabel
            Left = 8
            Top = 82
            Width = 95
            Height = 15
            Caption = 'Messages &Format:'
            FocusControl = edMessagesFormat
          end
          object btnStdFormats: TButton
            Left = 397
            Top = 78
            Width = 19
            Height = 20
            Anchors = [akTop, akRight]
            Caption = '<'
            DropDownMenu = FormatsPopup
            TabOrder = 6
            OnClick = btnStdFormatsClick
          end
          object cbCaptureOutput: TCheckBox
            Left = 271
            Top = 13
            Width = 141
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '&Capture Output'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object cbParseMessages: TCheckBox
            Left = 271
            Top = 33
            Width = 141
            Height = 21
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Anchors = [akTop, akRight]
            Caption = 'Parse &Messages'
            Checked = True
            State = cbChecked
            TabOrder = 3
            OnClick = cbParseMessagesClick
          end
          object cbParseTraceback: TCheckBox
            Left = 271
            Top = 54
            Width = 141
            Height = 21
            Hint = 
              'If checked output lines that look like messages '#13#10'will be parsed' +
              ' and shown in the messages window.'
            Anchors = [akTop, akRight]
            Caption = 'Parse &Traceback'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object edMessagesFormat: TEdit
            Left = 136
            Top = 79
            Width = 257
            Height = 21
            Hint = 
              'A grep expression for parsing output lines.'#13#10'Use the button on t' +
              'he right to insert '#13#10'predefined grep expressions.'
            Anchors = [akLeft, akTop, akRight]
            AutoSelect = False
            AutoSize = False
            TabOrder = 5
          end
          object cbStandardInput: TComboBox
            Left = 90
            Top = 20
            Width = 140
            Height = 23
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
            Left = 90
            Top = 49
            Width = 140
            Height = 23
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
          Left = 6
          Top = 378
          Width = 428
          Height = 44
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Console'
          TabOrder = 4
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
          object cbUTF8IO: TCheckBox
            Left = 270
            Top = 18
            Width = 203
            Height = 21
            Hint = 'Use the UTF8 encoding for stdin/stdout'
            Caption = '&UTF8 IO'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
        end
      end
    end
  end
  object FormatsPopup: TPopupMenu
    Left = 26
    Top = 452
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
    Images = vilImages
    OnUpdate = ActionListUpdate
    Left = 90
    Top = 452
    object actAddItem: TAction
      Caption = '&Add'
      Hint = 'Add item'
      ImageIndex = 2
      ImageName = 'Plus'
      OnExecute = actAddItemExecute
    end
    object actDeleteItem: TAction
      Caption = '&Delete'
      Hint = 'Delete item'
      ImageIndex = 0
      ImageName = 'Delete'
      OnExecute = actDeleteItemExecute
    end
    object actUpdateItem: TAction
      Caption = '&Update'
      Hint = 'Update item'
      ImageIndex = 1
      ImageName = 'Refresh'
      OnExecute = actUpdateItemExecute
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 21
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 88
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 68
        CollectionName = 'Plus'
        Name = 'Plus'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Left = 141
    Top = 452
  end
end
