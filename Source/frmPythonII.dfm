inherited PythonIIForm: TPythonIIForm
  Left = 259
  Top = 201
  HelpContext = 410
  Caption = 'Python Interpreter'
  ClientHeight = 451
  ClientWidth = 701
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000060000001A000000210000
    0021000000180000000500000000000000000000000000000000000000000000
    0000000000000000000000000000E0E0E023F0F0F0C3FCFCFCF8FFFFFFFFFBFB
    FBF8E2E2E2CD5B5B5B5000000006000000000000000000000000000000000000
    0000000000000000000000000000FDFDFDB8B3F5FFFF5DE3FFFF48D9FFFF49D3
    FFFFA0E6FFFFE2E2E2CF0000001C000000000000000000000000000000000000
    0000000000000000000000000000FFFFFFF665EDFFFF5CE8FFFF51E0FFFFD5F6
    FFFF3FD0FFFFFCFCFCF900000028000000000000000000000000000000000000
    0000000000060000001900000021FFFFFFFF62EDFFFF61EDFFFF5AE7FFFF4FDE
    FFFF44D6FFFFFFFFFFFF0000003B00000020000000170000000500000000DFDF
    DF23F0F0F0C2FCFCFCF8FFFFFFFFFFFFFFFF62EDFFFF62EDFFFFBEF7FFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBF8E2E2E2CD5A5A5A5000000006FDFD
    FDB9CCBAA5FF886A46FF7D6445FFFFFFFFFF63EDFFFF62EDFFFF62EDFFFF60EB
    FFFF56E3FFFF4BDBFFFF40D2FFFF39CBFFFF9DE4FFFFE2E2E2CF0000001DFFFF
    FFF6A17A4BFF8F6C41FF846743FFF1EEEAFF9BF4FFFF63EDFFFF62EDFFFF62ED
    FFFF5EEAFFFF54E2FFFF49D9FFFF3ED1FFFF40CCFFFFFCFCFCF900000029FFFF
    FFFFA4763DFF99713FFF8D6B42FFA38E74FFF0EEEAFFFFFFFFFFFFFFFFFFFFFF
    FFFFEDFDFFFF85EDFFFF52E0FFFF47D8FFFF3CCFFFFFFFFFFFFF0000002AFFFF
    FFF5B28247FFA2753DFF966F40FF8B6A42FF806544FF7C6345FF7C6345FF7D64
    46FFAC9C89FFEDFDFFFF5BE7FFFF4FDFFFFF50D9FFFFFBFBFBF800000021FFFF
    FFB6DCC2A2FFAD7C3FFFA0743EFF946E40FF896942FF7E6445FF7C6345FF7C63
    45FF7D6547FFFFFFFFFF61ECFFFF5CE6FFFFACEFFFFFEBEBEBC40000000B0000
    0000FFFFFFB8FFFFFFF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9BEB1FF7C63
    45FF7C6345FFFFFFFFFFFFFFFFFFFEFEFEF5FAFAFAB8BFBFBF25000000000000
    0000000000000000000000000000FFFFFFFF9B723FFF906C41FF856743FF7D63
    45FF7C6345FFFFFFFFFF00000029000000000000000000000000000000000000
    0000000000000000000000000000FFFFFFF5A67940FFE8DFD3FF8E6B41FF8266
    44FF7F6649FFFBFBFBF800000021000000000000000000000000000000000000
    0000000000000000000000000000FFFFFFB6D8C0A3FFA87D49FF977040FF9373
    4DFFC4B7A8FFEBEBEBC40000000B000000000000000000000000000000000000
    000000000000000000000000000000000000FFFFFFB8FFFFFFF7FFFFFFFFFEFE
    FEF5FAFAFAB9BFBFBF250000000000000000000000000000000000000000F81F
    9C41F00F9C41F00F9C41F00F9C4180019C4100009C4100009C4100009C410000
    9C4100009C4100009C4180019C41F00F9C41F00F9C41F00F9C41F81F9C41}
  Position = poDefault
  OnHelp = FormHelp
  ExplicitWidth = 717
  ExplicitHeight = 490
  PixelsPerInch = 96
  TextHeight = 13
  inherited BGPanel: TSpTBXPanel
    Width = 701
    Height = 451
    ExplicitWidth = 701
    ExplicitHeight = 451
    inherited FGPanel: TPanel
      Width = 697
      Height = 447
      Color = clInactiveBorder
      ExplicitWidth = 697
      ExplicitHeight = 447
      object SynEdit: TSynEdit
        Left = 0
        Top = 0
        Width = 697
        Height = 447
        HelpContext = 410
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = InterpreterPopUp
        TabOrder = 0
        OnDblClick = SynEditDblClick
        OnEnter = SynEditEnter
        OnExit = SynEditExit
        OnMouseDown = SynEditMouseDown
        OnMouseWheelDown = SynEditMouseWheelDown
        OnMouseWheelUp = SynEditMouseWheelUp
        CodeFolding.GutterShapeSize = 11
        CodeFolding.CollapsedLineColor = clGrayText
        CodeFolding.FolderBarLinesColor = clGrayText
        CodeFolding.IndentGuidesColor = clGray
        CodeFolding.IndentGuides = True
        CodeFolding.ShowCollapsedLine = False
        CodeFolding.ShowHintMark = True
        UseCodeFolding = False
        BorderStyle = bsNone
        Gutter.BorderStyle = gbsNone
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Gutter.Width = 0
        Gutter.Gradient = True
        Options = [eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent, eoTabsToSpaces]
        RightEdge = 0
        TabWidth = 4
        WantTabs = True
        WordWrap = True
        OnCommandProcessed = SynEditCommandProcessed
        OnProcessCommand = SynEditProcessCommand
        OnProcessUserCommand = SynEditProcessUserCommand
        OnPaintTransient = SynEditPaintTransient
        FontSmoothing = fsmNone
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 40
  end
  object PythonEngine: TPythonEngine
    AutoLoad = False
    DllName = 'python24.dll'
    APIVersion = 1012
    RegVersion = '2.4'
    FatalAbort = False
    FatalMsgDlg = False
    UseLastKnownVersion = False
    AutoFinalize = False
    InitThreads = True
    IO = PythonIO
    PyFlags = [pfInteractive]
    OnAfterInit = PythonEngineAfterInit
    Left = 493
    Top = 54
  end
  object PythonIO: TPythonInputOutput
    UnicodeIO = False
    RawOutput = False
    Left = 554
    Top = 53
  end
  object SynCodeCompletion: TSynCompletionProposal
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 200
    EndOfTokenChr = '()[]{}. =:'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Resizeable = False
    Images = CommandsDataModule.CodeImages
    OnClose = SynCodeCompletionClose
    OnExecute = SynCodeCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    OnAfterCodeCompletion = SynCodeCompletionAfterCodeCompletion
    Left = 523
    Top = 1
  end
  object DebugIDE: TPythonModule
    Engine = PythonEngine
    Events = <
      item
        Name = 'user_call'
      end
      item
        Name = 'user_line'
      end
      item
        Name = 'user_return'
      end
      item
        Name = 'user_exception'
        DocString.Strings = (
          'Return the 8087 CW value')
      end
      item
        Name = 'user_yield'
      end
      item
        Name = 'InputBox'
        OnExecute = InputBoxExecute
      end
      item
        Name = 'statusWrite'
        OnExecute = StatusWriteExecute
      end
      item
        Name = 'messageWrite'
        OnExecute = MessageWriteExecute
      end
      item
        Name = 'get8087CW'
        OnExecute = Get8087CWExecute
      end
      item
        Name = 'maskFPUexceptions'
        OnExecute = MaskFPUExceptionsExecute
      end
      item
        Name = 'unmaskFPUexceptions'
        OnExecute = UnMaskFPUExceptionsExecute
      end
      item
        Name = 'testResultStartTest'
        OnExecute = testResultStartTestExecute
      end
      item
        Name = 'testResultStopTest'
        OnExecute = testResultStopTestExecute
      end
      item
        Name = 'testResultAddSuccess'
        OnExecute = testResultAddSuccess
      end
      item
        Name = 'testResultAddFailure'
        OnExecute = testResultAddFailure
      end
      item
        Name = 'testResultAddError'
        OnExecute = testResultAddError
      end
      item
        Name = 'awakeGUI'
        OnExecute = awakeGUIExecute
      end>
    ModuleName = 'DebugIDE'
    Errors = <>
    Left = 612
    Top = 54
  end
  object SynParamCompletion: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. ='
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Images = CommandsDataModule.CodeImages
    OnExecute = SynParamCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    Left = 625
  end
  object InterpreterPopUp: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    Left = 37
    Top = 60
    object TBXPythonEngines: TSpTBXSubmenuItem
      Caption = 'Python Engine'
      LinkSubitems = PyIDEMainForm.mnPythonEngines
    end
    object TBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object mnEditCut: TSpTBXItem
      Action = CommandsDataModule.actEditCut
    end
    object mnEditCopy: TSpTBXItem
      Action = CommandsDataModule.actEditCopy
    end
    object mnEditPaste: TSpTBXItem
      Action = CommandsDataModule.actEditPaste
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnCopyNoPrompts: TSpTBXItem
      Action = actCopyWithoutPrompts
    end
    object mnPasteWithPrompts: TSpTBXItem
      Action = actPasteAndExecute
    end
    object mnCopyHistory: TSpTBXItem
      Action = actCopyHistory
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClearAll: TSpTBXItem
      Action = actClearContents
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnInterpreterEditorOptions: TSpTBXItem
      Action = CommandsDataModule.actInterpreterEditorOptions
    end
  end
  object PyDelphiWrapper: TPyDelphiWrapper
    Engine = PythonEngine
    Module = PyscripterModule
    Left = 636
    Top = 118
  end
  object PyscripterModule: TPythonModule
    Engine = PythonEngine
    ModuleName = 'pyscripter'
    Errors = <>
    Left = 544
    Top = 119
  end
  object InterpreterActionList: TActionList
    Images = CommandsDataModule.Images
    Left = 40
    Top = 109
    object actCopyHistory: TAction
      Category = 'Interpreter'
      Caption = 'Copy &History'
      HelpContext = 410
      Hint = 'Copy history to Clipboard'
      ImageIndex = 12
      OnExecute = actCopyHistoryExecute
    end
    object actClearContents: TAction
      Category = 'Interpreter'
      Caption = 'Clear &All'
      HelpContext = 410
      Hint = 'Clear all interpreter output'
      ImageIndex = 14
      OnExecute = actClearContentsExecute
    end
    object actCopyWithoutPrompts: TAction
      Category = 'Interpreter'
      Caption = 'Copy (&No Prompts)'
      HelpContext = 410
      Hint = 'Copy selected text without the interpreter prompts'
      ImageIndex = 12
      ShortCut = 24643
      OnExecute = actCopyWithoutPromptsExecute
    end
    object actPasteAndExecute: TAction
      Category = 'Interpreter'
      Caption = 'Paste && E&xecute'
      HelpContext = 410
      Hint = 'Paste clipboard text with added interpreter prompts'
      ImageIndex = 13
      ShortCut = 24662
      OnExecute = actPasteAndExecuteExecute
    end
  end
end
