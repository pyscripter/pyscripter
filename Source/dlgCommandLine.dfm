inherited CommandLineDlg: TCommandLineDlg
  HelpContext = 910
  ActiveControl = SynParameters
  Caption = 'Command Line Parameters'
  ClientHeight = 172
  ClientWidth = 528
  OnDestroy = FormDestroy
  ExplicitWidth = 534
  ExplicitHeight = 201
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 528
    Height = 172
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    ExplicitHeight = 170
    DesignSize = (
      528
      172)
    object SynParameters: TSynEdit
      Left = 11
      Top = 84
      Width = 484
      Height = 18
      Anchors = [akLeft, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      TabOrder = 0
      OnEnter = SynParametersEnter
      CodeFolding.GutterShapeSize = 11
      CodeFolding.CollapsedLineColor = clGrayText
      CodeFolding.FolderBarLinesColor = clGrayText
      CodeFolding.IndentGuidesColor = clGray
      CodeFolding.IndentGuides = True
      CodeFolding.ShowCollapsedLine = False
      CodeFolding.ShowHintMark = True
      UseCodeFolding = False
      FontSmoothing = fsmNone
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
      ExplicitTop = 82
    end
    object OKButton: TSpTBXButton
      Left = 144
      Top = 137
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      OnClick = OKButtonClick
      Default = True
      ModalResult = 1
      ExplicitTop = 135
    end
    object BitBtn2: TSpTBXButton
      Left = 238
      Top = 137
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Cancel = True
      ModalResult = 2
      ExplicitTop = 135
    end
    object HelpButton: TSpTBXButton
      Left = 333
      Top = 137
      Width = 75
      Height = 25
      Caption = '&Help'
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
      ExplicitTop = 135
    end
    object TBXButton1: TSpTBXButton
      Left = 501
      Top = 83
      Width = 17
      Height = 19
      Hint = 'History'
      Anchors = [akRight, akBottom]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      DropDownMenu = TBXPopupHistory
      ExplicitTop = 81
    end
    object cbUseCommandLine: TSpTBXCheckBox
      Left = 13
      Top = 16
      Width = 177
      Height = 21
      Caption = 'Use Command Line Parameters?'
      ParentColor = True
      TabOrder = 4
    end
    object Label1: TSpTBXLabel
      Left = 13
      Top = 43
      Width = 507
      Height = 32
      Caption = 
        'Please enter parameters to be appended to the command line:'#13#10'Not' +
        'e that the script name is automatically inserted as the first ar' +
        'gument.'
      Anchors = [akLeft, akRight, akBottom]
      Wrapping = twWrap
    end
    object Label3: TSpTBXLabel
      Left = 13
      Top = 108
      Width = 258
      Height = 19
      Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
      Anchors = [akLeft, akBottom]
      Enabled = False
    end
  end
  object TBXPopupHistory: TSpTBXPopupMenu
    OnPopup = TBXPopupHistoryPopup
    Left = 456
    Top = 8
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
