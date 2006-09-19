object UnitTestWizard: TUnitTestWizard
  Left = 0
  Top = 0
  HelpContext = 930
  BorderStyle = bsDialog
  Caption = 'Unit Test Wizard'
  ClientHeight = 449
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 449
    Align = alClient
    TabOrder = 0
    DesignSize = (
      400
      449)
    object Bevel1: TBevel
      Left = 3
      Top = 8
      Width = 393
      Height = 49
      Shape = bsFrame
      Style = bsRaised
    end
    object Label1: TLabel
      Left = 4
      Top = 65
      Width = 323
      Height = 13
      Caption = 
        'Select the functions and methods for which tests will be generat' +
        'ed:'
    end
    object lbHeader: TLabel
      Left = 16
      Top = 17
      Width = 325
      Height = 13
      Caption = 'This wizard will generate unit tests for the Python module'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ExplorerTree: TVirtualStringTree
      Left = 3
      Top = 83
      Width = 395
      Height = 319
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelKind = bkSoft
      BorderStyle = bsNone
      BorderWidth = 2
      CheckImageKind = ckXP
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintMode = hmHint
      Images = CommandsDataModule.CodeImages
      IncrementalSearch = isAll
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning, toVariableNodeHeight]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnGetText = ExplorerTreeGetText
      OnGetImageIndex = ExplorerTreeGetImageIndex
      OnGetHint = ExplorerTreeGetHint
      OnInitChildren = ExplorerTreeInitChildren
      OnInitNode = ExplorerTreeInitNode
      Columns = <>
    end
    object OKButton: TBitBtn
      Left = 63
      Top = 413
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      TabOrder = 1
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 159
      Top = 413
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      TabOrder = 2
      Kind = bkCancel
    end
    object HelpButton: TBitBtn
      Left = 255
      Top = 413
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = HelpButtonClick
      Kind = bkHelp
    end
  end
end
