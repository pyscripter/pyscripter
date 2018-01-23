inherited OptionsInspector: TOptionsInspector
  Left = 437
  Top = 134
  Caption = 'Options Inspector'
  ClientHeight = 349
  ClientWidth = 508
  OnDestroy = FormDestroy
  ExplicitWidth = 514
  ExplicitHeight = 378
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 508
    Height = 312
    Align = alClient
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object Inspector: TzObjectInspector
      Left = 1
      Top = 1
      Width = 506
      Height = 310
      Align = alClient
      Text = 'Inspector'
      Color = clWhite
      BorderStyle = bsSingle
      Component = Inspector
      TabOrder = 0
      AllowSearch = True
      AutoCompleteText = True
      DefaultCategoryName = 'Miscellaneous'
      ShowGutter = True
      GutterColor = clCream
      GutterEdgeColor = clGray
      NameColor = clBtnText
      ValueColor = clNavy
      NonDefaultValueColor = clNavy
      BoldNonDefaultValue = True
      HighlightColor = 14737632
      ReferencesColor = clMaroon
      SubPropertiesColor = clGreen
      ShowHeader = False
      ShowGridLines = False
      GridColor = clBlack
      SplitterColor = clGray
      ReadOnlyColor = clGrayText
      FixedSplitter = False
      ReadOnly = False
      TrackChange = False
      GutterWidth = 12
      ShowItemHint = True
      SortByCategory = True
      SplitterPos = 250
      HeaderPropText = 'Property'
      HeaderValueText = 'Value'
      ObjectVisibility = mvPublished
      FloatPreference.MaxDigits = 2
      FloatPreference.ExpPrecision = 6
      OnGetItemFriendlyName = InspectorGetItemFriendlyName
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 312
    Width = 508
    Height = 37
    Align = alBottom
    Anchors = [akLeft, akBottom]
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      508
      37)
    object OKButton: TButton
      Left = 217
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OKButtonClick
    end
    object BitBtn2: TButton
      Left = 313
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 409
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
  end
end
