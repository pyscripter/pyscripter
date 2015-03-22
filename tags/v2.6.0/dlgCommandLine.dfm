inherited CommandLineDlg: TCommandLineDlg
  HelpContext = 910
  ActiveControl = SynParameters
  Caption = 'Command Line Parameters'
  ClientHeight = 150
  ClientWidth = 446
  OnDestroy = FormDestroy
  ExplicitWidth = 452
  ExplicitHeight = 178
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 150
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      446
      150)
    object SynParameters: TSynEdit
      Left = 8
      Top = 61
      Width = 405
      Height = 18
      Anchors = [akLeft, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
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
    object OKButton: TSpTBXButton
      Left = 100
      Top = 115
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      OnClick = OKButtonClick
      Default = True
      ModalResult = 1
    end
    object BitBtn2: TSpTBXButton
      Left = 194
      Top = 115
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Cancel = True
      ModalResult = 2
    end
    object HelpButton: TSpTBXButton
      Left = 289
      Top = 115
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object TBXButton1: TSpTBXButton
      Left = 419
      Top = 61
      Width = 17
      Height = 19
      Hint = 'History'
      Anchors = [akRight, akBottom]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      DropDownMenu = TBXPopupHistory
    end
    object cbUseCommandLine: TSpTBXCheckBox
      Left = 8
      Top = 5
      Width = 177
      Height = 21
      Caption = 'Use Command Line Parameters?'
      ParentColor = True
      TabOrder = 4
    end
    object Label1: TSpTBXLabel
      Left = 8
      Top = 27
      Width = 421
      Height = 28
      Caption = 
        'Please enter parameters to be appended to the command line:'#13#10'Not' +
        'e that the script name is automatically inserted as the first ar' +
        'gument.'
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Wrapping = twWrap
    end
    object Label3: TSpTBXLabel
      Left = 8
      Top = 87
      Width = 258
      Height = 19
      Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
      Anchors = [akLeft, akBottom]
      Enabled = False
    end
  end
  object TBXPopupHistory: TSpTBXPopupMenu
    OnPopup = TBXPopupHistoryPopup
    Left = 392
    Top = 96
    object EmptyHistoryPopupItem: TSpTBXItem
      Caption = '(Empty History)'
    end
    object mnCommandHistoryMRU: TSpTBXMRUListItem
      HidePathExtension = False
      MaxItems = 6
      OnClick = mnCommandHistoryMRUClick
    end
  end
end
