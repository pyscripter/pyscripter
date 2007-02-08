object NewFileDialog: TNewFileDialog
  Left = 0
  Top = 0
  HelpContext = 640
  BorderStyle = bsDialog
  Caption = 'New File'
  ClientHeight = 297
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 466
    Height = 297
    Align = alClient
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 464
      Height = 255
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 185
        Top = 0
        Height = 255
        ResizeStyle = rsUpdate
        ExplicitLeft = 234
        ExplicitTop = 25
        ExplicitHeight = 100
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 255
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 185
          Height = 13
          Align = alTop
          Caption = 'Categories:'
          ExplicitWidth = 56
        end
        object tvCategories: TVirtualStringTree
          Left = 0
          Top = 13
          Width = 185
          Height = 242
          Align = alClient
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Shell Dlg 2'
          Header.Font.Style = []
          Header.MainColumn = -1
          Header.Options = [hoColumnResize, hoDrag]
          NodeDataSize = 0
          TabOrder = 0
          OnChange = tvCategoriesChange
          OnGetText = tvCategoriesGetText
          Columns = <>
        end
      end
      object Panel4: TPanel
        Left = 188
        Top = 0
        Width = 276
        Height = 255
        Align = alClient
        TabOrder = 1
        object Label2: TLabel
          Left = 1
          Top = 1
          Width = 274
          Height = 13
          Align = alTop
          Caption = 'Templates:'
          ExplicitWidth = 53
        end
        object lvTemplates: TListView
          Left = 1
          Top = 14
          Width = 274
          Height = 240
          Align = alClient
          Columns = <>
          FlatScrollBars = True
          ReadOnly = True
          TabOrder = 0
          OnDblClick = lvTemplatesDblClick
          OnSelectItem = lvTemplatesSelectItem
        end
      end
    end
    object btnCancel: TButton
      Left = 378
      Top = 264
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnCreate: TButton
      Left = 293
      Top = 265
      Width = 75
      Height = 25
      Caption = 'C&reate'
      Enabled = False
      TabOrder = 2
      OnClick = btnCreateClick
    end
    object btnManageTemplates: TButton
      Left = 11
      Top = 264
      Width = 144
      Height = 25
      Caption = 'Manage File Templates...'
      TabOrder = 3
      OnClick = btnManageTemplatesClick
    end
  end
end
