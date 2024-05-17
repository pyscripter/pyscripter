inherited LLMChatForm: TLLMChatForm
  Left = 0
  Top = 0
  Caption = 'Chat'
  ClientHeight = 655
  ClientWidth = 586
  OnShow = FormShow
  TextHeight = 15
  inherited BGPanel: TPanel
    Top = 34
    Width = 586
    Height = 621
    inherited FGPanel: TPanel
      Width = 582
      Height = 617
      object pnlQuestion: TPanel
        Left = 0
        Top = 532
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
          IndicatorType = aitSectorRing
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
          TextHint = 'Ask me anything'
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
          ScrollBars = ssVertical
          SelectedColor.Alpha = 0.400000005960464500
          WordWrap = True
        end
      end
      object ScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 582
        Height = 527
        HorzScrollBar.Visible = False
        VertScrollBar.Tracking = True
        Align = alClient
        ParentBackground = True
        TabOrder = 1
        UseWheelForScrolling = True
        object QAStackPanel: TStackPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 574
          Height = 23
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          ControlCollection = <>
          DoubleBuffered = True
          HorizontalPositioning = sphpFill
          ParentColor = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
      end
      object Splitter: TSpTBXSplitter
        Left = 0
        Top = 527
        Width = 582
        Height = 5
        Cursor = crSizeNS
        Align = alBottom
        ParentColor = False
        MinSize = 90
      end
    end
  end
  object SpTBXDock: TSpTBXDock [1]
    Left = 0
    Top = 0
    Width = 586
    Height = 34
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
      object SpTBXItem3: TSpTBXItem
        Action = actChatNew
      end
      object SpTBXItem4: TSpTBXItem
        Action = actChatRemove
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object spiSave: TSpTBXItem
        Action = actChatSave
      end
      object SpTBXRightAlignSpacerItem: TSpTBXRightAlignSpacerItem
        CustomWidth = 382
      end
      object SpTBXItem2: TSpTBXItem
        Action = actChatPrevious
      end
      object SpTBXItem1: TSpTBXItem
        Action = actChatNext
      end
      object SpTBXSubmenuItem1: TSpTBXSubmenuItem
        Caption = 'Settings:'
        ImageIndex = 2
        ImageName = 'Setup'
        Options = [tboDropdownArrow]
        OnInitPopup = SpTBXSubmenuItem1InitPopup
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
          EditCaption = 'Api key: '
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
        CollectionIndex = 103
        CollectionName = 'Setup'
        Name = 'Setup'
      end
      item
        CollectionIndex = 99
        CollectionName = 'Save'
        Name = 'Save'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 142
        CollectionName = 'Chat\Chat'
        Name = 'Chat'
      end
      item
        CollectionIndex = 143
        CollectionName = 'Chat\ChatPlus'
        Name = 'ChatPlus'
      end
      item
        CollectionIndex = 144
        CollectionName = 'Chat\ChatRemove'
        Name = 'ChatRemove'
      end
      item
        CollectionIndex = 145
        CollectionName = 'Chat\ChatQuestion'
        Name = 'ChatQuestion'
      end
      item
        CollectionIndex = 146
        CollectionName = 'Chat\UserQuestion'
        Name = 'UserQuestion'
      end
      item
        CollectionIndex = 147
        CollectionName = 'Chat\ChatGPT'
        Name = 'ChatGPT'
      end
      item
        CollectionIndex = 65
        CollectionName = 'Paste'
        Name = 'Paste'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    Width = 24
    Height = 24
    Left = 32
    Top = 504
  end
  object ActionList: TActionList
    Images = vilImages
    OnUpdate = ActionListUpdate
    Left = 32
    Top = 440
    object actChatSave: TAction
      Caption = 'Save chat'
      Hint = 'Save chat history'
      ImageIndex = 3
      ImageName = 'Save'
      OnExecute = actChatSaveExecute
    end
    object actChatRemove: TAction
      Caption = 'Remove Chat Topic'
      Hint = 'Remove current chat topic'
      ImageIndex = 7
      ImageName = 'ChatRemove'
      OnExecute = actChatRemoveExecute
    end
    object actChatNew: TAction
      Caption = 'New Chat Topic'
      Hint = 'Add a new chat topic'
      ImageIndex = 6
      ImageName = 'ChatPlus'
      OnExecute = actChatNewExecute
    end
    object actChatPrevious: TAction
      Caption = 'Previous Chat Topic'
      Hint = 'Show previous chat topic'
      ImageIndex = 0
      ImageName = 'ArrowLeft'
      OnExecute = actChatPreviousExecute
    end
    object actChatNext: TAction
      Caption = 'Next Chat Topic'
      Hint = 'Show next chat topic'
      ImageIndex = 1
      ImageName = 'ArrowRight'
      OnExecute = actChatNextExecute
    end
    object actCopyText: TAction
      Caption = 'Copy '
      Hint = 'Copy text'
      ImageIndex = 4
      ImageName = 'Copy'
      OnExecute = actCopyTextExecute
    end
    object actAskQuestion: TAction
      Hint = 'Ask question'
      ImageIndex = 8
      ImageName = 'ChatQuestion'
      OnExecute = actAskQuestionExecute
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 32
    Top = 320
  end
  object SynMultiSyn: TSynMultiSyn
    Schemes = <
      item
        StartExpr = '```'
        EndExpr = '```'
        MarkerAttri.Background = clNone
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
  object pmTextMenu: TSpTBXPopupMenu
    Images = vilImages
    Left = 32
    Top = 152
    object mnCopyText: TSpTBXItem
      Action = actCopyText
    end
  end
end
