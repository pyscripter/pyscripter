inherited fmToDoOptions: TfmToDoOptions
  Left = 347
  Top = 241
  HelpContext = 465
  Caption = 'To Do Options'
  ClientHeight = 283
  ClientWidth = 512
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  ExplicitWidth = 518
  ExplicitHeight = 312
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBG: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 283
    Align = alClient
    TabOrder = 0
    DesignSize = (
      512
      283)
    object gbxTokens: TGroupBox
      Left = 8
      Top = 9
      Width = 233
      Height = 271
      Caption = 'To Do Tokens'
      TabOrder = 0
      DesignSize = (
        233
        271)
      object lblPriority: TLabel
        Left = 131
        Top = 202
        Width = 34
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = '&Priority'
        Color = clNone
        FocusControl = cboPriority
        ParentColor = False
      end
      object lblToken: TLabel
        Left = 131
        Top = 149
        Width = 29
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'To&ken'
        Color = clNone
        FocusControl = edToken
        ParentColor = False
      end
      object edToken: TEdit
        Left = 131
        Top = 168
        Width = 96
        Height = 21
        Anchors = [akRight, akBottom]
        TabOrder = 4
        OnChange = edTokenChange
      end
      object cboPriority: TComboBox
        Left = 131
        Top = 221
        Width = 96
        Height = 21
        Style = csDropDownList
        Anchors = [akRight, akBottom]
        TabOrder = 5
        OnChange = cboPriorityChange
        Items.Strings = (
          'High'
          'Normal'
          'Low')
      end
      object lstTokens: TListBox
        Left = 7
        Top = 20
        Width = 118
        Height = 243
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lstTokensClick
      end
      object btnInsert: TButton
        Left = 140
        Top = 30
        Width = 77
        Height = 26
        Caption = '&Insert'
        TabOrder = 1
        OnClick = btnInsertClick
      end
      object btnApply: TButton
        Left = 140
        Top = 62
        Width = 77
        Height = 26
        Caption = '&Apply'
        TabOrder = 2
        OnClick = btnApplyClick
      end
      object btnRemove: TButton
        Left = 140
        Top = 94
        Width = 77
        Height = 26
        Caption = '&Remove'
        TabOrder = 3
        OnClick = btnRemoveClick
      end
    end
    object gbxOptions: TGroupBox
      Left = 251
      Top = 7
      Width = 253
      Height = 48
      Caption = 'Options'
      TabOrder = 1
      object cbShowTokens: TCheckBox
        Left = 8
        Top = 19
        Width = 230
        Height = 21
        Caption = '&Show tokens in description'
        TabOrder = 0
      end
    end
    object gbxSearchFiles: TGroupBox
      Left = 247
      Top = 61
      Width = 253
      Height = 184
      Caption = 'Search for To Do Tokens'
      TabOrder = 2
      DesignSize = (
        253
        184)
      object meDirectories: TRichEdit
        Left = 10
        Top = 89
        Width = 208
        Height = 66
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
        Zoom = 100
      end
      object chkInclude: TCheckBox
        Left = 10
        Top = 160
        Width = 230
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = 'Include su&bdirectories'
        TabOrder = 3
      end
      object radScanOpen: TRadioButton
        Left = 10
        Top = 15
        Width = 230
        Height = 21
        Caption = 'Scan &open files'
        TabOrder = 0
        TabStop = True
        OnClick = radScanDirClick
      end
      object radScanDir: TRadioButton
        Left = 10
        Top = 60
        Width = 230
        Height = 21
        Caption = 'Scan &directories'
        TabOrder = 1
        TabStop = True
        OnClick = radScanDirClick
      end
      object radScanProject: TRadioButton
        Left = 10
        Top = 38
        Width = 230
        Height = 21
        Caption = 'Scan &project files'
        TabOrder = 5
        TabStop = True
        OnClick = radScanDirClick
      end
      object btnBrowse: TButton
        Left = 224
        Top = 87
        Width = 20
        Height = 20
        Hint = 'Select directory'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnBrowseClick
      end
    end
    object btnOK: TButton
      Left = 254
      Top = 251
      Width = 80
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
    end
    object btnCancel: TButton
      Left = 342
      Top = 251
      Width = 80
      Height = 26
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnHelp: TButton
      Left = 430
      Top = 252
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 5
      OnClick = btnHelpClick
    end
  end
end
