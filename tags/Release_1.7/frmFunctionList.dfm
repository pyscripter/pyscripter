object FunctionListWindow: TFunctionListWindow
  Left = 284
  Top = 176
  Width = 525
  Height = 351
  HelpContext = 820
  ActiveControl = edtMethods
  Anchors = [akLeft, akTop, akRight]
  BorderIcons = [biSystemMenu]
  Caption = 'Function List'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
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
  PixelsPerInch = 96
  TextHeight = 13
  object pnHolder: TPanel
    Left = 0
    Top = 58
    Width = 517
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object lvProcs: TListView
      Left = 0
      Top = 0
      Width = 517
      Height = 240
      Align = alClient
      Columns = <
        item
          Width = 20
        end
        item
          Caption = 'Procedure'
          Width = 313
        end
        item
          Alignment = taRightJustify
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 298
    Width = 517
    Height = 19
    Panels = <
      item
        Width = 420
      end
      item
        Text = '999/999'
        Width = 50
      end>
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 22
    Width = 517
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    FullRepaint = False
    ParentColor = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnResize = pnlHeaderResize
    object pnlHeaderLeft: TPanel
      Left = 0
      Top = 0
      Width = 260
      Height = 36
      Align = alLeft
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object lblMethods: TLabel
        Left = 14
        Top = 10
        Width = 34
        Height = 13
        Alignment = taRightJustify
        Caption = '&Search'
        FocusControl = edtMethods
      end
      object edtMethods: TEdit
        Left = 53
        Top = 6
        Width = 200
        Height = 21
        TabOrder = 0
        OnChange = edtMethodsChange
        OnKeyDown = edtMethodsKeyDown
        OnKeyPress = edtMethodsKeyPress
      end
    end
    object pnlHeaderRight: TPanel
      Left = 260
      Top = 0
      Width = 257
      Height = 36
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 1
      object lblObjects: TLabel
        Left = 16
        Top = 8
        Width = 36
        Height = 13
        Alignment = taRightJustify
        Caption = '&Objects'
        FocusControl = cbxObjects
      end
      object cbxObjects: TComboBox
        Left = 56
        Top = 4
        Width = 196
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
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 517
    Height = 22
    AutoSize = True
    DisabledImages = CommandsDataModule.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = CommandsDataModule.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Wrapable = False
    object tbnCopy: TToolButton
      Left = 0
      Top = 0
      Action = actEditCopy
    end
    object tbnSep1: TToolButton
      Left = 23
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnFont: TToolButton
      Left = 31
      Top = 0
      Action = actOptionsFont
    end
    object tbnSep2: TToolButton
      Left = 54
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnStart: TToolButton
      Left = 62
      Top = 0
      Action = actViewStart
      Grouped = True
      Style = tbsCheck
    end
    object tbnAny: TToolButton
      Left = 85
      Top = 0
      Action = actViewAny
      Grouped = True
      Style = tbsCheck
    end
    object tbnSep3: TToolButton
      Left = 108
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object tbnGoto: TToolButton
      Left = 116
      Top = 0
      Action = actViewGoto
    end
    object tbnSep4: TToolButton
      Left = 139
      Top = 0
      Width = 8
      ImageIndex = 5
      Style = tbsSeparator
    end
    object ToolButton1: TToolButton
      Left = 147
      Top = 0
      Action = actHelpHelp
    end
  end
  object dlgProcFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
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
