object WebPreviewForm: TWebPreviewForm
  Left = 417
  Top = 88
  HelpContext = 850
  Caption = 'Web Preview'
  ClientHeight = 446
  ClientWidth = 463
  Color = clBtnFace
  ParentFont = True
  PixelsPerInch = 96
  TextHeight = 15
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 30
    Width = 463
    Height = 416
    HelpContext = 850
    Align = alClient
    TabOrder = 0
    OnCommandStateChange = WebBrowserCommandStateChange
    ControlData = {
      4C000000DA2F0000FF2A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object TBXDock1: TSpTBXDock
    Left = 0
    Top = 0
    Width = 463
    Height = 30
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
        ImageName = 'Back'
        OnClick = ToolButtonBackClick
      end
      object ToolButtonForward: TSpTBXItem
        Hint = 'Go Forward'
        ImageIndex = 1
        ImageName = 'Forward'
        OnClick = ToolButtonForwardClick
      end
      object TBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object TBXItem3: TSpTBXItem
        Hint = 'Stop'
        ImageIndex = 2
        ImageName = 'Cancel'
        OnClick = ToolButtonStopClick
      end
      object TBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object TBXItem4: TSpTBXItem
        Hint = 'Page Setup'
        ImageIndex = 3
        ImageName = 'PageSetup'
        OnClick = ToolButtonPageSetupClick
      end
      object TBXItem6: TSpTBXItem
        Hint = 'Print Preview'
        ImageIndex = 4
        ImageName = 'Preview'
        OnClick = ToolButtonPrintPreviewClick
      end
      object TBXItem5: TSpTBXItem
        Hint = 'Print'
        ImageIndex = 5
        ImageName = 'Print'
        OnClick = ToolButtonPrintClick
      end
      object TBXSeparatorItem4: TSpTBXSeparatorItem
      end
      object TBXItem7: TSpTBXItem
        Hint = 'Save'
        ImageIndex = 6
        ImageName = 'Save'
        OnClick = ToolButtonSaveClick
      end
    end
  end
  object BrowserImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Browser\Back'
        Name = 'Back'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Browser\Forward'
        Name = 'Forward'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Browser\Cancel'
        Name = 'Cancel'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Browser\PageSetup'
        Name = 'PageSetup'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Browser\Preview'
        Name = 'Preview'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Browser\Print'
        Name = 'Print'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Browser\Save'
        Name = 'Save'
      end>
    ImageCollection = CommandsDataModule.icBrowserImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 40
    Top = 96
  end
end
