inherited SpTBXCustomizeFormMod: TSpTBXCustomizeFormMod
  PixelsPerInch = 96
  TextHeight = 13
  inherited SpTBXTabControl1: TSpTBXTabControl
    SkinType = sknSkin
    HiddenItems = <>
    inherited tabToolbars: TSpTBXTabItem
      SkinType = sknSkin
    end
    inherited tabCommands: TSpTBXTabItem
      SkinType = sknSkin
    end
    inherited tabShortcuts: TSpTBXTabItem
      Visible = False
      SkinType = sknSkin
    end
    inherited SpTBXTabSheet3: TSpTBXTabSheet
      TabVisible = False
      TabItem = 'tabShortcuts'
    end
    inherited SpTBXTabSheet1: TSpTBXTabSheet
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
        Width = 66
        Height = 13
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
        Width = 64
        Height = 13
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
        ItemHeight = 16
        Sorted = True
        TabOrder = 4
        OnClick = lbCategoriesClick
      end
    end
    inherited SpTBXTabSheet2: TSpTBXTabSheet
      TabItem = 'tabToolbars'
    end
  end
end
