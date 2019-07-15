inherited SpTBXCustomizeFormMod: TSpTBXCustomizeFormMod
  ClientHeight = 325
  ClientWidth = 449
  ParentFont = True
  ExplicitWidth = 465
  ExplicitHeight = 364
  PixelsPerInch = 96
  TextHeight = 13
  inherited SpTBXTabControl1: TSpTBXTabControl
    Width = 449
    Height = 290
    ExplicitWidth = 432
    ExplicitHeight = 281
    HiddenItems = <>
    inherited tabShortcuts: TSpTBXTabItem
      Visible = False
    end
    inherited SpTBXTabSheet2: TSpTBXTabSheet
      Width = 449
      Height = 265
      TabVisible = False
      ExplicitWidth = 432
      ExplicitHeight = 256
      TabItem = 'tabShortcuts'
      inherited SpTBXPanel5: TSpTBXPanel
        Top = 231
        Width = 349
        ExplicitTop = 222
        ExplicitWidth = 332
        inherited Panel1: TPanel
          Width = 345
          ExplicitWidth = 328
          inherited HotKey1: THotKey
            Width = 351
            ExplicitWidth = 334
          end
        end
      end
      inherited ChangeShortcut: TSpTBXButton
        Left = 363
        Top = 230
        ExplicitLeft = 346
        ExplicitTop = 221
      end
      inherited lbShortcuts: TSpTBXListBox
        Width = 432
        Height = 212
        ExplicitWidth = 415
        ExplicitHeight = 203
      end
    end
    inherited SpTBXTabSheet3: TSpTBXTabSheet
      Width = 449
      Height = 265
      ExplicitWidth = 432
      ExplicitHeight = 256
      TabItem = 'tabCommands'
      inherited SpTBXLabel3: TSpTBXLabel
        Left = 3
        Top = 207
        Width = 432
        Height = 52
        ExplicitLeft = 3
        ExplicitTop = 207
        ExplicitWidth = 432
        ExplicitHeight = 52
      end
      inherited lbCommands: TSpTBXListBox
        Left = 208
        Top = 25
        Width = 230
        Height = 176
        Anchors = [akTop, akRight, akBottom]
        ExplicitLeft = 208
        ExplicitTop = 25
        ExplicitWidth = 230
        ExplicitHeight = 176
      end
      object SpTBXLabel2: TSpTBXLabel
        Left = 191
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
      ExplicitWidth = 432
      ExplicitHeight = 256
      TabItem = 'tabToolbars'
      inherited SpTBXGroupBox1: TSpTBXGroupBox
        Left = 255
        Top = 8
        Width = 183
        Height = 251
        ExplicitLeft = 255
        ExplicitTop = 8
        ExplicitWidth = 183
        ExplicitHeight = 251
        inherited cbText: TSpTBXComboBox
          Left = 16
          Top = 68
          Width = 150
          ExplicitLeft = 16
          ExplicitTop = 68
          ExplicitWidth = 150
        end
        inherited cbIcon: TSpTBXComboBox
          Left = 16
          Top = 120
          Width = 150
          ExplicitLeft = 16
          ExplicitTop = 120
          ExplicitWidth = 150
        end
        inherited cbTextLabel: TSpTBXLabel
          Left = 11
          Top = 43
          ExplicitLeft = 11
          ExplicitTop = 43
        end
        inherited cbIconLabel: TSpTBXLabel
          Left = 14
          ExplicitLeft = 14
        end
        inherited checkVisible: TSpTBXCheckBox
          Left = 14
          Top = 20
          ExplicitLeft = 14
          ExplicitTop = 20
        end
      end
      inherited SpTBXGroupBox2: TSpTBXGroupBox
        Left = 261
        Top = 178
        Width = 172
        Visible = False
        ExplicitLeft = 261
        ExplicitTop = 174
        ExplicitWidth = 172
        inherited cbSkins: TSpTBXComboBox
          Left = 3
          Top = 25
          ExplicitLeft = 3
          ExplicitTop = 25
        end
      end
      inherited lbToolbars: TSpTBXCheckListBox
        Left = 11
        Top = 3
        Width = 238
        Height = 247
        ExplicitLeft = 11
        ExplicitTop = 3
        ExplicitWidth = 238
        ExplicitHeight = 243
      end
    end
  end
  inherited ClosePanel: TSpTBXPanel
    Top = 290
    Width = 449
    TBXStyleBackground = False
    ExplicitTop = 281
    ExplicitWidth = 432
    object Panel2: TPanel [0]
      Left = 0
      Top = 0
      Width = 449
      Height = 35
      Align = alClient
      ParentBackground = False
      TabOrder = 2
      ExplicitWidth = 432
    end
    inherited CloseButton: TSpTBXButton
      Left = 366
      ExplicitLeft = 349
    end
    inherited ResetButton: TSpTBXButton
      Top = 3
      ExplicitTop = 3
    end
  end
end
