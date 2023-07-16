inherited SpTBXCustomizeFormMod: TSpTBXCustomizeFormMod
  ClientHeight = 325
  ClientWidth = 449
  Font.Height = -12
  Font.Name = 'Segoe UI'
  TextHeight = 15
  inherited SpTBXTabControl1: TSpTBXTabControl
    Width = 449
    Height = 290
    HiddenItems = <>
    inherited tabShortcuts: TSpTBXTabItem
      Visible = False
    end
    inherited SpTBXTabSheet2: TSpTBXTabSheet
      Width = 449
      Height = 265
      TabVisible = False
      TabItem = 'tabShortcuts'
      inherited SpTBXPanel5: TSpTBXPanel
        Top = 231
        Width = 349
        inherited Panel1: TPanel
          Width = 345
          inherited HotKey1: THotKey
            Width = 351
            Height = 23
          end
        end
      end
      inherited ChangeShortcut: TSpTBXButton
        Left = 363
        Top = 230
      end
      inherited lbShortcuts: TSpTBXListBox
        Width = 432
        Height = 212
      end
    end
    inherited SpTBXTabSheet3: TSpTBXTabSheet
      Width = 449
      Height = 265
      TabItem = 'tabCommands'
      inherited SpTBXLabel3: TSpTBXLabel
        Left = 3
        Top = 207
        Width = 432
        Height = 52
      end
      inherited lbCommands: TSpTBXListBox
        Left = 208
        Top = 28
        Width = 230
        Height = 173
      end
      object SpTBXLabel2: TSpTBXLabel
        Left = 208
        Top = 3
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
        Top = 3
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
        Top = 28
        Width = 195
        Height = 173
        Anchors = [akLeft, akTop, akBottom]
        Sorted = True
        TabOrder = 4
        OnClick = lbCategoriesClick
      end
    end
    inherited SpTBXTabSheet1: TSpTBXTabSheet
      Width = 449
      Height = 265
      TabItem = 'tabToolbars'
      inherited SpTBXGroupBox1: TSpTBXGroupBox
        Left = 255
        Top = 8
        Width = 183
        Height = 251
        inherited cbText: TSpTBXComboBox
          Left = 16
          Top = 68
          Width = 150
          Height = 23
          ItemHeight = 15
        end
        inherited cbIcon: TSpTBXComboBox
          Left = 16
          Top = 120
          Width = 150
          Height = 23
          ItemHeight = 15
        end
        inherited cbTextLabel: TSpTBXLabel
          Left = 11
          Top = 43
          Width = 72
          Height = 21
        end
        inherited cbIconLabel: TSpTBXLabel
          Left = 14
          Width = 74
          Height = 21
        end
        inherited checkVisible: TSpTBXCheckBox
          Left = 14
          Top = 20
          Width = 58
          Height = 23
        end
      end
      inherited SpTBXGroupBox2: TSpTBXGroupBox
        Left = 261
        Top = 178
        Width = 172
        Visible = False
        inherited cbSkins: TSpTBXComboBox
          Left = 3
          Top = 25
          Height = 23
          ItemHeight = 15
        end
      end
      inherited lbToolbars: TSpTBXCheckListBox
        Left = 11
        Top = 3
        Width = 238
        Height = 247
      end
    end
  end
  inherited ClosePanel: TSpTBXPanel
    Top = 290
    Width = 449
    TBXStyleBackground = False
    object Panel2: TPanel [0]
      Left = 0
      Top = 0
      Width = 449
      Height = 35
      Align = alClient
      ParentBackground = False
      TabOrder = 2
    end
    inherited CloseButton: TSpTBXButton
      Left = 366
    end
    inherited ResetButton: TSpTBXButton
      Top = 3
    end
  end
end
