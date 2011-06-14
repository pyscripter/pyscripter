inherited fmToDoOptions: TfmToDoOptions
  Left = 347
  Top = 241
  HelpContext = 465
  Caption = 'To Do Options'
  ClientHeight = 270
  ClientWidth = 512
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  ExplicitWidth = 518
  ExplicitHeight = 298
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBG: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 270
    Caption = 'pnlBG'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      512
      270)
    object gbxTokens: TSpTBXGroupBox
      Left = 8
      Top = 6
      Width = 233
      Height = 256
      Caption = 'To Do Tokens'
      TabOrder = 0
      TBXStyleBackground = True
      object btnInsert: TSpTBXButton
        Left = 140
        Top = 30
        Width = 77
        Height = 26
        Caption = '&Insert'
        TabOrder = 1
        OnClick = btnInsertClick
      end
      object btnApply: TSpTBXButton
        Left = 140
        Top = 62
        Width = 77
        Height = 26
        Caption = '&Apply'
        TabOrder = 2
        OnClick = btnApplyClick
      end
      object btnRemove: TSpTBXButton
        Left = 140
        Top = 94
        Width = 77
        Height = 26
        Caption = '&Remove'
        TabOrder = 3
        OnClick = btnRemoveClick
      end
      object lstTokens: TSpTBXListBox
        Left = 7
        Top = 20
        Width = 118
        Height = 228
        Style = lbStandard
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lstTokensClick
      end
      object edToken: TSpTBXEdit
        Left = 131
        Top = 155
        Width = 96
        Height = 21
        TabOrder = 4
        OnChange = edTokenChange
      end
      object cboPriority: TSpTBXComboBox
        Left = 131
        Top = 203
        Width = 96
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        OnChange = cboPriorityChange
        Items.Strings = (
          'High'
          'Normal'
          'Low')
      end
      object lblPriority: TSpTBXLabel
        Left = 131
        Top = 182
        Width = 40
        Height = 19
        Caption = '&Priority'
        FocusControl = cboPriority
      end
      object lblToken: TSpTBXLabel
        Left = 131
        Top = 133
        Width = 35
        Height = 19
        Caption = 'To&ken'
        FocusControl = edToken
      end
    end
    object gbxOptions: TSpTBXGroupBox
      Left = 251
      Top = 7
      Width = 253
      Height = 48
      Caption = 'Options'
      TabOrder = 1
      TBXStyleBackground = True
      object cbShowTokens: TSpTBXCheckBox
        Left = 8
        Top = 19
        Width = 151
        Height = 21
        Caption = '&Show tokens in description'
        ParentColor = True
        TabOrder = 0
      end
    end
    object gbxSearchFiles: TSpTBXGroupBox
      Left = 251
      Top = 62
      Width = 253
      Height = 171
      Caption = 'Search for To Do Tokens'
      TabOrder = 2
      TBXStyleBackground = True
      object btnBrowse: TSpTBXButton
        Left = 223
        Top = 76
        Width = 20
        Height = 20
        Hint = 'Select Directory'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnBrowseClick
      end
      object chkInclude: TSpTBXCheckBox
        Left = 8
        Top = 149
        Width = 129
        Height = 21
        Caption = 'Include su&bdirectories'
        ParentColor = True
        TabOrder = 3
      end
      object radScanOpen: TSpTBXRadioButton
        Left = 10
        Top = 17
        Width = 96
        Height = 21
        Caption = 'Scan &open files'
        ParentColor = True
        TabOrder = 0
        TabStop = True
        OnClick = radScanDirClick
      end
      object radScanDir: TSpTBXRadioButton
        Left = 10
        Top = 55
        Width = 100
        Height = 21
        Caption = 'Scan &directories'
        ParentColor = True
        TabOrder = 1
        TabStop = True
        OnClick = radScanDirClick
      end
      object radScanProject: TSpTBXRadioButton
        Left = 10
        Top = 36
        Width = 106
        Height = 21
        Caption = 'Scan &project files'
        ParentColor = True
        TabOrder = 5
        TabStop = True
        OnClick = radScanDirClick
      end
      object meDirectories: TRichEdit
        Left = 9
        Top = 78
        Width = 208
        Height = 66
        Font.Charset = GREEK_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
      end
    end
    object btnOK: TSpTBXButton
      Left = 254
      Top = 238
      Width = 80
      Height = 26
      Caption = '&OK'
      TabOrder = 3
      Default = True
      ModalResult = 1
    end
    object btnCancel: TSpTBXButton
      Left = 342
      Top = 238
      Width = 80
      Height = 26
      Caption = '&Cancel'
      TabOrder = 4
      Cancel = True
      ModalResult = 2
    end
    object btnHelp: TSpTBXButton
      Left = 430
      Top = 239
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 5
      OnClick = btnHelpClick
    end
  end
end
