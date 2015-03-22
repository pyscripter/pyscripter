inherited FunctionListWindow: TFunctionListWindow
  Left = 284
  Top = 176
  HelpContext = 820
  ActiveControl = edtMethods
  Anchors = [akLeft, akTop, akRight]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Function List'
  ClientHeight = 312
  ClientWidth = 525
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
  OnClose = FormClose
  OnKeyPress = edtMethodsKeyPress
  ExplicitWidth = 541
  ExplicitHeight = 350
  PixelsPerInch = 96
  TextHeight = 13
  object pnHolder: TSpTBXPanel
    Left = 0
    Top = 69
    Width = 525
    Height = 218
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    object lvProcs: TEasyListview
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 515
      Height = 208
      Align = alClient
      EditManager.Font.Charset = DEFAULT_CHARSET
      EditManager.Font.Color = clWindowText
      EditManager.Font.Height = -11
      EditManager.Font.Name = 'Tahoma'
      EditManager.Font.Style = []
      Header.Columns.Items = {
        0600000003000000110000005445617379436F6C756D6E53746F726564FFFECE
        0006000000800800010100010000000000000114000000FFFFFF1F0001000000
        00000000000000000000000000000000110000005445617379436F6C756D6E53
        746F726564FFFECE00060000008008000101010101000000000001A4010000FF
        FFFF1F00010000000100000008000000460075006E006300740069006F006E00
        000000000000000000000000110000005445617379436F6C756D6E53746F7265
        64FFFECE0006000000800801010100010200000000000146000000FFFFFF1F00
        0100000001000000040000004C0069006E006500000000000000000000000000}
      Header.Visible = True
      ImagesSmall = CommandsDataModule.CodeImages
      PaintInfoGroup.MarginBottom.CaptionIndent = 4
      Selection.FullRowSelect = True
      Sort.AutoSort = True
      TabOrder = 0
      View = elsReport
      OnItemCompare = lvProcsItemCompare
      OnItemDblClick = lvProcsItemDblClick
      OnItemSelectionsChanged = lvProcsItemSelectionsChanged
      OnSortEnd = lvProcsSortEnd
    end
  end
  object pnlHeader: TSpTBXPanel
    Left = 0
    Top = 26
    Width = 525
    Height = 43
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnResize = pnlHeaderResize
    TBXStyleBackground = True
    object pnlHeaderLeft: TSpTBXPanel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 260
      Height = 33
      Align = alLeft
      TabOrder = 0
      TBXStyleBackground = True
      object lblMethods: TSpTBXLabel
        Left = 10
        Top = 8
        Width = 39
        Height = 19
        Caption = '&Search'
        Alignment = taRightJustify
        FocusControl = edtMethods
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
      AlignWithMargins = True
      Left = 271
      Top = 5
      Width = 249
      Height = 33
      Align = alClient
      TabOrder = 1
      TBXStyleBackground = True
      object lblObjects: TSpTBXLabel
        Left = 10
        Top = 8
        Width = 43
        Height = 19
        Caption = '&Objects'
        Alignment = taRightJustify
        FocusControl = cbxObjects
      end
      object cbxObjects: TSpTBXComboBox
        Left = 82
        Top = 6
        Width = 170
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
    Width = 525
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
        Action = actEditCopy
      end
      object SpTBXSeparatorItem4: TSpTBXSeparatorItem
      end
      object tbiFont: TSpTBXItem
        Action = actOptionsFont
      end
      object SpTBXSeparatorItem3: TSpTBXSeparatorItem
      end
      object tbiViewStart: TSpTBXItem
        Action = actViewStart
      end
      object tbiViewAny: TSpTBXItem
        Action = actViewAny
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object tbiViewGoto: TSpTBXItem
        Action = actViewGoto
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object tbiHelp: TSpTBXItem
        Action = actHelpHelp
      end
    end
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 287
    Width = 525
    Height = 25
    ParentShowHint = False
    ShowHint = True
    object LeftStatusLabel: TSpTBXLabelItem
      Wrapping = twEndEllipsis
      Options = [tboShowHint]
      CustomHeight = 21
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      CustomWidth = 490
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object RightStatusLabel: TSpTBXLabelItem
      Wrapping = twNone
      Alignment = taCenter
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
  object Actions: TActionList
    Images = CommandsDataModule.Images
    OnUpdate = ActionsUpdate
    Left = 32
    Top = 136
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Edit'
      Hint = 'Copy procedures to clipboard'
      ImageIndex = 12
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actOptionsFont: TAction
      Category = 'Options'
      Caption = '&Font..'
      Hint = 'Configure font'
      ImageIndex = 91
      OnExecute = actOptionsFontExecute
    end
    object actViewStart: TAction
      Category = 'View'
      Caption = 'S&tart'
      Hint = 'Match only from the start'
      ImageIndex = 94
      OnExecute = actViewStartExecute
    end
    object actViewAny: TAction
      Category = 'View'
      Caption = '&Any'
      Hint = 'Match anywhere'
      ImageIndex = 93
      OnExecute = actViewAnyExecute
    end
    object actViewGoto: TAction
      Category = 'View'
      Caption = '&Goto'
      Hint = 'Goto implementation'
      ImageIndex = 32
      OnExecute = actViewGotoExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 33
      OnExecute = actHelpHelpExecute
    end
  end
end
