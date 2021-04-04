object DocForm: TDocForm
  Left = 417
  Top = 88
  HelpContext = 850
  Caption = 'Documentation'
  ClientHeight = 446
  ClientWidth = 463
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 34
    Width = 463
    Height = 412
    HelpContext = 850
    Align = alClient
    TabOrder = 0
    OnCommandStateChange = WebBrowserCommandStateChange
    ControlData = {
      4C000000DA2F0000952A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object TBXDock1: TSpTBXDock
    Left = 0
    Top = 0
    Width = 463
    Height = 34
    AllowDrag = False
    DoubleBuffered = True
    object TBXToolbar1: TSpTBXToolbar
      Left = 0
      Top = 0
      DockPos = 0
      FullSize = True
      Images = BrowserImages
      TabOrder = 0
      Caption = 'TBXToolbar1'
      Customizable = False
      object ToolButtonBack: TSpTBXItem
        Hint = 'Go Back'
        ImageIndex = 0
        OnClick = ToolButtonBackClick
      end
      object ToolButtonForward: TSpTBXItem
        Hint = 'Go Forward'
        ImageIndex = 1
        OnClick = ToolButtonForwardClick
      end
      object TBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object TBXItem3: TSpTBXItem
        Hint = 'Stop'
        ImageIndex = 2
        OnClick = ToolButtonStopClick
      end
      object TBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object TBXItem4: TSpTBXItem
        Hint = 'Page Setup'
        ImageIndex = 3
        OnClick = ToolButtonPageSetupClick
      end
      object TBXItem6: TSpTBXItem
        Hint = 'Print Preview'
        ImageIndex = 4
        OnClick = ToolButtonPrintPreviewClick
      end
      object TBXItem5: TSpTBXItem
        Hint = 'Print'
        ImageIndex = 5
        OnClick = ToolButtonPrintClick
      end
      object TBXSeparatorItem4: TSpTBXSeparatorItem
      end
      object TBXItem7: TSpTBXItem
        Hint = 'Save'
        ImageIndex = 6
        OnClick = ToolButtonSaveClick
      end
    end
  end
  object BrowserImages: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Browser\Back'
        Disabled = False
        Name = 'Back'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Browser\Forward'
        Disabled = False
        Name = 'Forward'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Browser\Cancel'
        Disabled = False
        Name = 'Cancel'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Browser\PageSetup'
        Disabled = False
        Name = 'PageSetup'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Browser\Preview'
        Disabled = False
        Name = 'Preview'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Browser\Print'
        Disabled = False
        Name = 'Print'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Browser\Save'
        Disabled = False
        Name = 'Save'
      end>
    ImageCollection = CommandsDataModule.icBrowserImages
    PreserveItems = True
    Width = 24
    Height = 24
    Left = 40
    Top = 88
  end
end
