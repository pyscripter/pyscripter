object CommandLineDlg: TCommandLineDlg
  Left = 0
  Top = 0
  HelpContext = 910
  ActiveControl = SynParameters
  BorderStyle = bsDialog
  Caption = 'Command Line Parameters'
  ClientHeight = 151
  ClientWidth = 458
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
    Width = 458
    Height = 151
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 435
    DesignSize = (
      458
      151)
    object Label1: TLabel
      Left = 8
      Top = 28
      Width = 345
      Height = 26
      Anchors = [akLeft, akBottom]
      Caption = 
        'Please enter parameters to be appended to the command line:'#13#10'Not' +
        'e that the scipt name is automatically inserted as the first arg' +
        'ument.'
    end
    object Label3: TLabel
      Left = 8
      Top = 88
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
    end
    object SynParameters: TSynEdit
      Left = 8
      Top = 67
      Width = 415
      Height = 18
      Anchors = [akLeft, akBottom]
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
    end
    object OKButton: TBitBtn
      Left = 112
      Top = 116
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      TabOrder = 1
      OnClick = OKButtonClick
      Kind = bkOK
      ExplicitLeft = 89
    end
    object BitBtn2: TBitBtn
      Left = 206
      Top = 116
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      TabOrder = 2
      Kind = bkCancel
      ExplicitLeft = 183
    end
    object HelpButton: TBitBtn
      Left = 301
      Top = 116
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
      Kind = bkHelp
      ExplicitLeft = 278
    end
    object cbUseCommandLine: TCheckBox
      Left = 8
      Top = 8
      Width = 241
      Height = 17
      Caption = 'Use Command Line Parameters?'
      TabOrder = 4
    end
    object TBXButton1: TTBXButton
      Left = 429
      Top = 67
      Width = 16
      Height = 16
      Hint = 'History'
      DropDownMenu = TBXPopupHistory
      ImageIndex = -1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
  end
  object TBXPopupHistory: TTBXPopupMenu
    OnPopup = TBXPopupHistoryPopup
    Left = 424
    Top = 96
    object EmptyHistoryPopupItem: TTBXItem
      Caption = '(Empty History)'
    end
    object PopupHistoryItem: TTBXMRUListItem
      MRUList = CommandsDataModule.CommandLineMRU
    end
  end
end
