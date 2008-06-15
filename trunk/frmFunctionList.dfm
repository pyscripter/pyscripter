inherited FunctionListWindow: TFunctionListWindow
  Left = 284
  Top = 176
  HelpContext = 820
  ActiveControl = edtMethods
  Anchors = [akLeft, akTop, akRight]
  BorderIcons = [biSystemMenu]
  Caption = 'Function List'
  ClientHeight = 315
  ClientWidth = 529
  Font.Name = 'MS Shell Dlg 2'
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000FF030000FF030000FF030000
    FF030000FF0300000000000000000000FF030000FF030000FF030000FF030000
    FF030000FF060000FF060000FF060000FF090000FF090000FF040000FF070000
    FF070000FF060000FF060000FF060000FF060000FF070000FF070000FF040000
    FF060000FF0A0000FF080000FF080000FF100000FF0D0000FF050000FFFF0000
    FF0B0000FF070000FF0A0000FF0A0000FF070000FF0B0000FFFF0000FF050000
    FF090000FF0E0000FFFF0000FFFF0000FF170000FF110000FFFF0000FF0F0000
    FF0F0000FFFF0000FF0E0000FF0E0000FFFF0000FF0F0000FF0F0000FFFF0000
    FF0C0000FF120000FFFF0000FFFF0000FF1E0000FF150000FFFF0000FF150000
    FF180000FFFF0000FF150000FF150000FFFF0000FF150000FF120000FFFF0000
    FF0F0000FF180000FFFF0000FFFF0000FF270000FF180000FFFF0000FF150000
    FF180000FFFF0000FF110000FF130000FFFF0000FF170000FF120000FFFF0000
    FF120000FF1B0000FFFF0000FFFF0000FF270000FF1B0000FFFF0000FF120000
    FF170000FFFF0000FFFF0000FFFF0000FF0B0000FF160000FF0F0000FFFF0000
    FF100000FF190000FFFF0000FFFF0000FF220000FF180000FFFF0000FF0C0000
    FF110000FF0B0000FF090000FF0B0000FF0A0000FF100000FF090000FFFF0000
    FF100000FFFF0000FFFF0000FFFF0000FFFF0000FF170000FF080000FFFF0000
    FF0D0000FF0C0000FF090000FF090000FF090000FF0A0000FFFF0000FF050000
    FF100000FF190000FFFF0000FFFF0000FF1C0000FF160000FF070000FF040000
    FF040000FF0300000000000000000000FF030000FF040000FF040000FF040000
    FF120000FF1B0000FFFF0000FFFF0000FF210000FF1B0000FF0C0000FF090000
    FF060000FF0300000000000000000000FF030000FF030000FF030000FF030000
    FF0C0000FF150000FFFF0000FFFF0000FF1F0000FF170000FF080000FF070000
    FF03000000000000000000000000000000000000000000000000000000000000
    FF090000FF110000FFFF0000FFFF0000FF150000FFFF0000FFFF0000FF070000
    FF03000000000000000000000000000000000000000000000000000000000000
    FF060000FF0D0000FF0C0000FFFF0000FFFF0000FFFF0000FF060000FF070000
    FF03000000000000000000000000000000000000000000000000000000000000
    FF030000FF090000FF0A0000FF0E0000FF0F0000FF0E0000FF0A0000FF090000
    FF03000000000000000000000000000000000000000000000000000000000000
    00000000FF030000FF060000FF090000FF090000FF090000FF060000FF030000
    000000000000000000000000000000000000000000000000000000000000F830
    0000000000000000000000000000000000000000000000000000000000000000
    00000030000000300000007F0000007F0000007F0000007F000080FF0000}
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnKeyPress = edtMethodsKeyPress
  OnResize = FormResize
  ExplicitWidth = 535
  ExplicitHeight = 341
  PixelsPerInch = 96
  TextHeight = 13
  object pnHolder: TSpTBXPanel
    Left = 0
    Top = 62
    Width = 529
    Height = 228
    ThemeType = thtWindows
    Align = alClient
    TabOrder = 0
    object lvProcs: TTntListView
      Left = 2
      Top = 2
      Width = 525
      Height = 224
      Align = alClient
      Columns = <
        item
          Width = 20
        end
        item
          Caption = 'Function'
          Width = 313
        end
        item
          Caption = 'Line'
          Width = 68
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      SmallImages = CommandsDataModule.CodeImages
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvProcsChange
      OnColumnClick = lvProcsColumnClick
      OnDblClick = actViewGotoExecute
    end
  end
  object pnlHeader: TSpTBXPanel
    Left = 0
    Top = 26
    Width = 529
    Height = 36
    ThemeType = thtWindows
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnResize = pnlHeaderResize
    object pnlHeaderLeft: TSpTBXPanel
      Left = 2
      Top = 2
      Width = 260
      Height = 32
      ThemeType = thtWindows
      Align = alLeft
      TabOrder = 0
      object lblMethods: TSpTBXLabel
        Left = 14
        Top = 10
        Width = 33
        Height = 13
        Caption = '&Search'
        ParentColor = True
        Alignment = taRightJustify
        FocusControl = edtMethods
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object edtMethods: TSpTBXEdit
        Left = 74
        Top = 6
        Width = 179
        Height = 21
        TabOrder = 0
        OnChange = edtMethodsChange
        OnKeyDown = edtMethodsKeyDown
        OnKeyPress = edtMethodsKeyPress
      end
    end
    object pnlHeaderRight: TSpTBXPanel
      Left = 262
      Top = 2
      Width = 265
      Height = 32
      ThemeType = thtWindows
      Align = alClient
      TabOrder = 1
      object lblObjects: TSpTBXLabel
        Left = 16
        Top = 8
        Width = 37
        Height = 13
        Caption = '&Objects'
        ParentColor = True
        Alignment = taRightJustify
        FocusControl = cbxObjects
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object cbxObjects: TSpTBXComboBox
        Left = 78
        Top = 4
        Width = 174
        Height = 21
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnChange = cbxObjectsChange
        OnKeyPress = edtMethodsKeyPress
      end
    end
  end
  object ToolBarDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 529
    Height = 26
    AllowDrag = False
    LimitToOneRow = True
    object Toolbar: TSpTBXToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      FullSize = True
      Images = CommandsDataModule.Images
      TabOrder = 0
      Customizable = False
      object tbiCopy: TSpTBXItem
        Caption = '&Edit'
        Hint = 'Copy procedures to clipboard'
        Action = actEditCopy
      end
      object SpTBXSeparatorItem4: TSpTBXSeparatorItem
      end
      object tbiFont: TSpTBXItem
        Caption = '&Font..'
        Hint = 'Configure font'
        Action = actOptionsFont
      end
      object SpTBXSeparatorItem3: TSpTBXSeparatorItem
      end
      object tbiViewStart: TSpTBXItem
        Caption = 'S&tart'
        Hint = 'Match only from the start'
        Action = actViewStart
      end
      object tbiViewAny: TSpTBXItem
        Caption = '&Any'
        Hint = 'Match anywhere'
        Action = actViewAny
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object tbiViewGoto: TSpTBXItem
        Caption = '&Goto'
        Hint = 'Goto implementation'
        Action = actViewGoto
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object tbiHelp: TSpTBXItem
        Caption = '&Help'
        Hint = 'Help'
        Action = actHelpHelp
      end
    end
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 290
    Width = 529
    Height = 25
    ParentShowHint = False
    ShowHint = True
    object LeftStatusLabel: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      Options = [tboShowHint]
      CustomHeight = 21
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      CustomWidth = 433
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object RightStatusLabel: TSpTBXLabelItem
      Wrapping = twNone
      Alignment = taCenter
      CustomWidth = 80
    end
  end
  object dlgProcFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Left = 32
    Top = 88
  end
  object Actions: TTntActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionsUpdate
    Left = 32
    Top = 136
    object actEditCopy: TTntAction
      Category = 'Edit'
      Caption = '&Edit'
      Hint = 'Copy procedures to clipboard'
      ImageIndex = 12
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actOptionsFont: TTntAction
      Category = 'Options'
      Caption = '&Font..'
      Hint = 'Configure font'
      ImageIndex = 91
      OnExecute = actOptionsFontExecute
    end
    object actViewStart: TTntAction
      Category = 'View'
      Caption = 'S&tart'
      Hint = 'Match only from the start'
      ImageIndex = 94
      OnExecute = actViewStartExecute
    end
    object actViewAny: TTntAction
      Category = 'View'
      Caption = '&Any'
      Hint = 'Match anywhere'
      ImageIndex = 93
      OnExecute = actViewAnyExecute
    end
    object actViewGoto: TTntAction
      Category = 'View'
      Caption = '&Goto'
      Hint = 'Goto implementation'
      ImageIndex = 32
      OnExecute = actViewGotoExecute
    end
    object actHelpHelp: TTntAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 33
      OnExecute = actHelpHelpExecute
    end
  end
end
