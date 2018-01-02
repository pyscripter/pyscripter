inherited SpTBXCustomizeFormMod: TSpTBXCustomizeFormMod
  ParentFont = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited SpTBXTabControl1: TSpTBXTabControl
    HiddenItems = <>
    inherited tabShortcuts: TSpTBXTabItem
      Visible = False
    end
    inherited SpTBXTabSheet3: TSpTBXTabSheet
      TabItem = 'tabCommands'
      inherited lbCommands: TSpTBXListBox
        Left = 161
        Top = 25
        Width = 175
        Height = 187
        ExplicitLeft = 161
        ExplicitTop = 25
        ExplicitWidth = 175
        ExplicitHeight = 187
      end
      object SpTBXLabel2: TSpTBXLabel
        Left = 159
        Top = 6
        Width = 72
        Height = 19
        Caption = 'Commands:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object SpTBXLabel5: TSpTBXLabel
        Left = 10
        Top = 5
        Width = 70
        Height = 19
        Caption = 'Categories:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lbCategories: TSpTBXListBox
        Left = 7
        Top = 26
        Width = 148
        Height = 188
        Sorted = True
        TabOrder = 4
        OnClick = lbCategoriesClick
      end
    end
    inherited SpTBXTabSheet2: TSpTBXTabSheet
      TabVisible = False
      TabItem = 'tabShortcuts'
    end
    inherited SpTBXTabSheet1: TSpTBXTabSheet
      ExplicitTop = 23
      TabItem = 'tabToolbars'
      inherited SpTBXGroupBox1: TSpTBXGroupBox
        Height = 247
        ExplicitHeight = 247
      end
      inherited SpTBXGroupBox2: TSpTBXGroupBox
        Visible = False
      end
    end
  end
  inherited ClosePanel: TSpTBXPanel
    TBXStyleBackground = False
    object Panel2: TPanel [0]
      Left = 0
      Top = 0
      Width = 347
      Height = 35
      Align = alClient
      ParentBackground = False
      TabOrder = 2
      ExplicitLeft = 80
      ExplicitWidth = 185
      ExplicitHeight = 41
    end
    inherited ResetButton: TSpTBXButton
      Top = 3
      ExplicitTop = 3
    end
  end
end
