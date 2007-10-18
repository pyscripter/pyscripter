object CommandLineDlg: TCommandLineDlg
  Left = 0
  Top = 0
  HelpContext = 910
  ActiveControl = SynParameters
  Caption = 'Command Line Parameters'
  ClientHeight = 150
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 150
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 458
    ExplicitHeight = 151
    DesignSize = (
      446
      150)
    object Label1: TLabel
      Left = 8
      Top = 27
      Width = 345
      Height = 26
      Anchors = [akLeft, akBottom]
      Caption = 
        'Please enter parameters to be appended to the command line:'#13#10'Not' +
        'e that the scipt name is automatically inserted as the first arg' +
        'ument.'
      ExplicitTop = 28
    end
    object Label3: TLabel
      Left = 8
      Top = 87
      Width = 252
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Parameters : Shift+Ctrl+P, Modifiers : Shift+Ctrl+M '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGradientActiveCaption
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      ExplicitTop = 88
    end
    object SynParameters: TSynEdit
      Left = 8
      Top = 63
      Width = 405
      Height = 18
      Anchors = [akLeft, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Lucida Console'
      Font.Style = []
      TabOrder = 0
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
      ExplicitTop = 64
      ExplicitWidth = 408
    end
    object OKButton: TBitBtn
      Left = 100
      Top = 115
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 1
      OnClick = OKButtonClick
      Kind = bkOK
      ExplicitLeft = 112
      ExplicitTop = 116
    end
    object BitBtn2: TBitBtn
      Left = 194
      Top = 115
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 2
      Kind = bkCancel
      ExplicitLeft = 206
      ExplicitTop = 116
    end
    object HelpButton: TBitBtn
      Left = 289
      Top = 115
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
      Kind = bkHelp
      ExplicitLeft = 301
      ExplicitTop = 116
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
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
      ExplicitLeft = 422
      ExplicitTop = 64
    end
    object cbUseCommandLine: TSpTBXCheckBox
      Left = 8
      Top = 8
      Width = 171
      Height = 15
      Caption = 'Use Command Line Parameters?'
      ParentColor = True
      TabOrder = 4
    end
  end
  object TBXPopupHistory: TSpTBXPopupMenu
    OnPopup = TBXPopupHistoryPopup
    Left = 424
    Top = 96
    object EmptyHistoryPopupItem: TSpTBXItem
      Caption = '(Empty History)'
    end
    object PopupHistoryItem: TTBXMRUListItem
      MRUList = CommandsDataModule.CommandLineMRU
    end
  end
end
