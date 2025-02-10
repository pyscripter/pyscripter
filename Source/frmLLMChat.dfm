inherited LLMChatForm: TLLMChatForm
  Left = 0
  Top = 0
  HelpContext = 497
  Caption = 'Chat'
  ClientHeight = 655
  ClientWidth = 586
  OnShow = FormShow
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 586
    Height = 655
    FullRepaint = False
    inherited FGPanel: TPanel
      Width = 582
      Height = 651
      object pnlQuestion: TPanel
        Left = 0
        Top = 566
        Width = 582
        Height = 85
        Align = alBottom
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          582
          85)
        object sbAsk: TSpeedButton
          Left = 542
          Top = 4
          Width = 32
          Height = 32
          Action = actAskQuestion
          Anchors = [akTop, akRight]
          Images = vilImages
          Flat = True
        end
        object aiBusy: TActivityIndicator
          Left = 542
          Top = 44
          Anchors = [akRight, akBottom]
          FrameDelay = 150
          IndicatorType = aitRotatingSector
        end
        object synQuestion: TSynEdit
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 530
          Height = 77
          Cursor = crDefault
          Align = alLeft
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          PopupMenu = pmAsk
          TabOrder = 0
          OnEnter = synQuestionEnter
          OnKeyDown = synQuestionKeyDown
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Gutter.Font.Quality = fqClearTypeNatural
          Gutter.Visible = False
          Gutter.Bands = <
            item
              Kind = gbkMarks
              Width = 13
            end
            item
              Kind = gbkLineNumbers
            end
            item
              Kind = gbkFold
            end
            item
              Kind = gbkTrackChanges
            end
            item
              Kind = gbkMargin
              Width = 3
            end>
          HideSelection = True
          Highlighter = SynMultiSyn
          RightEdge = 0
          ScrollBars = ssVertical
          ScrollbarAnnotations = <
            item
              AnnType = sbaCarets
              AnnPos = sbpFullWidth
              FullRow = False
            end>
          VisibleSpecialChars = []
          WordWrap = True
        end
      end
      object Splitter: TSpTBXSplitter
        Left = 0
        Top = 561
        Width = 582
        Height = 5
        Cursor = crSizeNS
        Align = alBottom
        ParentColor = False
        MinSize = 90
      end
      object SpTBXDock: TSpTBXDock
        Left = 0
        Top = 0
        Width = 582
        Height = 34
        AllowDrag = False
        DoubleBuffered = True
        object SpTBXToolbar: TSpTBXToolbar
          Left = 0
          Top = 0
          CloseButton = False
          DockMode = dmCannotFloatOrChangeDocks
          DockPos = 0
          DragHandleStyle = dhNone
          FullSize = True
          Images = vilImages
          ParentShowHint = False
          ShowHint = True
          ShrinkMode = tbsmNone
          Stretch = True
          TabOrder = 0
          Customizable = False
          object spiNewTopic: TSpTBXItem
            Action = actChatNew
          end
          object spiRemoveTopic: TSpTBXItem
            Action = actChatRemove
          end
          object SpTBXSeparatorItem7: TSpTBXSeparatorItem
          end
          object spiPreviousTopic: TSpTBXItem
            Action = actChatPrevious
            ImageIndex = 16
            ImageName = 'ChatPrev'
          end
          object spiNextTopic: TSpTBXItem
            Action = actChatNext
            ImageIndex = 15
            ImageName = 'ChatNext'
          end
          object SpTBXSeparatorItem5: TSpTBXSeparatorItem
          end
          object spiTitle: TSpTBXItem
            Action = actTopicTitle
          end
          object SpTBXSeparatorItem2: TSpTBXSeparatorItem
          end
          object spiPrint: TTBItem
            Action = actPrint
          end
          object spiSave: TSpTBXItem
            Action = actChatSave
          end
          object SpTBXSeparatorItem4: TSpTBXSeparatorItem
          end
          object spiCancel: TTBItem
            Action = actCancelRequest
          end
          object SpTBXRightAlignSpacerItem: TSpTBXRightAlignSpacerItem
            CustomWidth = 267
          end
          object spiSettings: TSpTBXSubmenuItem
            Caption = 'Settings'
            HelpContext = 770
            ImageIndex = 2
            ImageName = 'Setup'
            Options = [tboDropdownArrow]
            OnInitPopup = spiSettingsInitPopup
            object spiDeepSeek: TSpTBXItem
              Caption = 'DeepSeek'
              Hint = 'Use DeepSeek'
              GroupIndex = 1
              ImageIndex = 18
              ImageName = 'DeepSeek'
              OnClick = mnProviderClick
              OnDrawImage = HighlightCheckedImg
            end
            object spiGemini: TSpTBXItem
              Caption = 'Gemini'
              Hint = 'Use Gemini'
              GroupIndex = 1
              ImageIndex = 20
              ImageName = 'Gemini'
              OnClick = mnProviderClick
              OnDrawImage = HighlightCheckedImg
            end
            object spiOpenai: TSpTBXItem
              Caption = 'OpenAI'
              Hint = 'Use OpenAI'
              AutoCheck = True
              Checked = True
              GroupIndex = 1
              ImageIndex = 19
              ImageName = 'OpenAI'
              OnClick = mnProviderClick
              OnDrawImage = HighlightCheckedImg
            end
            object spiOllama: TSpTBXItem
              Caption = 'Ollama'
              Hint = 'Use Ollama'
              AutoCheck = True
              GroupIndex = 1
              ImageIndex = 17
              ImageName = 'Ollama'
              OnClick = mnProviderClick
              OnDrawImage = HighlightCheckedImg
            end
            object SpTBXSeparatorItem6: TSpTBXSeparatorItem
            end
            object spiEndpoint: TSpTBXEditItem
              CustomWidth = 100
              EditCaption = 'Endpoint:'
              ExtendedAccept = True
              OnAcceptText = AcceptSettings
            end
            object spiModel: TSpTBXEditItem
              CustomWidth = 100
              EditCaption = 'Model:'
              ExtendedAccept = True
              OnAcceptText = AcceptSettings
            end
            object spiApiKey: TSpTBXEditItem
              CustomWidth = 300
              EditCaption = 'Api key:'
              ExtendedAccept = True
              PasswordChar = #9679
              OnAcceptText = AcceptSettings
            end
            object SpTBXSeparatorItem1: TSpTBXSeparatorItem
            end
            object spiTimeout: TSpTBXEditItem
              EditCaption = 'Timeout (in seconds):'
              ExtendedAccept = True
              OnAcceptText = AcceptSettings
            end
            object spiTemperature: TSpTBXEditItem
              EditCaption = 'Temperature (0.0 to 2.0):'
              ExtendedAccept = True
              OnAcceptText = AcceptSettings
            end
            object spiMaxTokens: TSpTBXEditItem
              EditCaption = 'Maximum number of response tokens:'
              ExtendedAccept = True
              OnAcceptText = AcceptSettings
            end
            object spiSystemPrompt: TSpTBXEditItem
              EditCaption = 'System prompt:'
              ExtendedAccept = True
              OnAcceptText = AcceptSettings
            end
          end
        end
      end
      object EdgeBrowser: TEdgeBrowser
        Left = 0
        Top = 34
        Width = 582
        Height = 527
        Align = alClient
        TabOrder = 4
        AllowSingleSignOnUsingOSPrimaryAccount = False
        TargetCompatibleBrowserVersion = '117.0.2045.28'
        UserDataFolder = '%LOCALAPPDATA%\bds.exe.WebView2'
        OnCreateWebViewCompleted = EdgeBrowserCreateWebViewCompleted
        OnNavigationCompleted = EdgeBrowserNavigationCompleted
        OnWebMessageReceived = EdgeBrowserWebMessageReceived
      end
      object pnlBrowserCover: TPanel
        Left = 0
        Top = 34
        Width = 582
        Height = 527
        Align = alClient
        TabOrder = 3
      end
    end
  end
  inherited DockClient: TJvDockClient
    LRDockWidth = 400
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 2
        CollectionName = 'ArrowLeft'
        Name = 'ArrowLeft'
      end
      item
        CollectionIndex = 3
        CollectionName = 'ArrowRight'
        Name = 'ArrowRight'
      end
      item
        CollectionIndex = 112
        CollectionName = 'Setup'
        Name = 'Setup'
      end
      item
        CollectionIndex = 108
        CollectionName = 'Save'
        Name = 'Save'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Chat\Chat'
        Name = 'Chat'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Chat\ChatPlus'
        Name = 'ChatPlus'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Chat\ChatRemove'
        Name = 'ChatRemove'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Chat\ChatQuestion'
        Name = 'ChatQuestion'
      end
      item
        CollectionIndex = 142
        CollectionName = 'Chat\UserQuestion'
        Name = 'UserQuestion'
      end
      item
        CollectionIndex = 74
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 130
        CollectionName = 'Title'
        Name = 'Title'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Assistant'
        Name = 'Assistant'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 94
        CollectionName = 'PythonScript'
        Name = 'PythonScript'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Chat\ChatNext'
        Name = 'ChatNext'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Chat\ChatPrev'
        Name = 'ChatPrev'
      end
      item
        CollectionIndex = 153
        CollectionName = 'LLMProviders\Ollama'
        Name = 'Ollama'
      end
      item
        CollectionIndex = 154
        CollectionName = 'LLMProviders\DeepSeek'
        Name = 'DeepSeek'
      end
      item
        CollectionIndex = 155
        CollectionName = 'LLMProviders\OpenAI'
        Name = 'OpenAI'
      end
      item
        CollectionIndex = 156
        CollectionName = 'LLMProviders\Gemini'
        Name = 'Gemini'
      end
      item
        CollectionIndex = 79
        CollectionName = 'Print'
        Name = 'Print'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    Width = 24
    Height = 24
    Left = 32
    Top = 504
  end
  object ChatActionList: TActionList
    Images = vilImages
    OnUpdate = ChatActionListUpdate
    Left = 32
    Top = 440
    object actChatSave: TAction
      Category = 'Chat'
      Caption = 'Save chat'
      Hint = 'Save chat history'
      ImageIndex = 3
      ImageName = 'Save'
      OnExecute = actChatSaveExecute
    end
    object actChatRemove: TAction
      Category = 'Chat'
      Caption = 'Remove Chat Topic'
      Hint = 'Remove current chat topic'
      ImageIndex = 7
      ImageName = 'ChatRemove'
      OnExecute = actChatRemoveExecute
    end
    object actChatNew: TAction
      Category = 'Chat'
      Caption = 'New Chat Topic'
      Hint = 'Add a new chat topic'
      ImageIndex = 6
      ImageName = 'ChatPlus'
      OnExecute = actChatNewExecute
    end
    object actChatPrevious: TAction
      Category = 'Chat'
      Caption = 'Previous Chat Topic'
      Hint = 'Show previous chat topic'
      ImageIndex = 0
      ImageName = 'ArrowLeft'
      OnExecute = actChatPreviousExecute
    end
    object actChatNext: TAction
      Category = 'Chat'
      Caption = 'Next Chat Topic'
      Hint = 'Show next chat topic'
      ImageIndex = 1
      ImageName = 'ArrowRight'
      OnExecute = actChatNextExecute
    end
    object actAskQuestion: TAction
      Category = 'Chat'
      Hint = 'Ask question'
      ImageIndex = 8
      ImageName = 'ChatQuestion'
      OnExecute = actAskQuestionExecute
    end
    object actTopicTitle: TAction
      Category = 'Chat'
      Caption = 'Topic Title'
      Hint = 'Set the title of the chat topic'
      ImageIndex = 11
      ImageName = 'Title'
      OnExecute = actTopicTitleExecute
    end
    object actCancelRequest: TAction
      Category = 'Chat'
      Caption = 'Cancel Request'
      Hint = 'Cancel active request'
      ImageIndex = 13
      ImageName = 'Delete'
      OnExecute = actCancelRequestExecute
    end
    object actPrint: TAction
      Category = 'Chat'
      Caption = 'Print'
      Hint = 'Print chat topic'
      ImageIndex = 21
      ImageName = 'Print'
      OnExecute = actPrintExecute
    end
  end
  object SynMultiSyn: TSynMultiSyn
    Schemes = <
      item
        StartExpr = '```python'
        EndExpr = '```'
        MarkerAttri.Background = clNone
        MarkerAttri.Style = []
        SchemeName = 'Python'
      end
      item
        StartExpr = '```'
        EndExpr = '```'
        MarkerAttri.Background = clNone
        MarkerAttri.Style = []
        SchemeName = 'Python'
      end>
    Left = 32
    Top = 264
  end
  object pmAsk: TSpTBXPopupMenu
    Left = 32
    Top = 208
    object mnCopy: TSpTBXItem
      Action = CommandsDataModule.actEditCopy
    end
    object mnPaste: TSpTBXItem
      Action = CommandsDataModule.actEditPaste
    end
    object SpTBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object mnSpelling: TSpTBXSubmenuItem
      Caption = 'Spelling'
      LinkSubitems = CommandsDataModule.mnSpelling
    end
  end
end
