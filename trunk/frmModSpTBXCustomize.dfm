inherited SpTBXCustomizeFormMod: TSpTBXCustomizeFormMod
  PixelsPerInch = 96
  TextHeight = 13
  inherited SpTBXTabControl1: TSpTBXTabControl
    HiddenItems = <>
    inherited tabShortcuts: TSpTBXTabItem
      Visible = False
    end
    inherited SpTBXTabSheet3: TSpTBXTabSheet [3]
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
    inherited SpTBXTabSheet2: TSpTBXTabSheet [4]
      TabVisible = False
      TabItem = 'tabShortcuts'
    end
    inherited SpTBXTabSheet1: TSpTBXTabSheet
      TabItem = 'tabToolbars'
    end
  end
  inherited ClosePanel: TSpTBXPanel
    inherited ResetButton: TSpTBXButton
      Top = 3
      ExplicitTop = 3
    end
  end
end
