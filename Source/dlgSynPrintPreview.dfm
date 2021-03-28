inherited PrintPreviewDlg: TPrintPreviewDlg
  Left = 192
  Top = 148
  ActiveControl = SynEditPrintPreview
  BorderStyle = bsSizeable
  Caption = 'Print Preview'
  ClientHeight = 374
  ClientWidth = 490
  Position = poDefault
  ShowHint = True
  StyleElements = [seFont, seClient]
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SynEditPrintPreview: TSynEditPrintPreview
    Left = 0
    Top = 26
    Width = 490
    Height = 338
    ScaleMode = pscWholePage
    OnMouseDown = SynEditPrintPreviewMouseDown
    OnPreviewPage = SynEditPrintPreviewPreviewPage
  end
  object ToolbarDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 490
    Height = 26
    AllowDrag = False
    object SpTBXToolbar1: TSpTBXToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      FullSize = True
      Images = vilPrintPreview
      TabOrder = 0
      Caption = 'SpTBXToolbar1'
      object tbiFirst: TSpTBXItem
        Action = FirstCmd
      end
      object tbiPrev: TSpTBXItem
        Action = PrevCmd
      end
      object tbiNext: TSpTBXItem
        Action = NextCmd
      end
      object tbiLast: TSpTBXItem
        Action = LastCmd
      end
      object SpTBXSeparatorItem3: TSpTBXSeparatorItem
      end
      object tbiZoom: TSpTBXSubmenuItem
        Action = ZoomCmd
        DropdownCombo = True
        object tbiWholePage: TSpTBXItem
          Tag = -1
          Caption = 'Whole page'
          Hint = 'Show whole page'
          OnClick = Fitto1Click
        end
        object tbiPageWidth: TSpTBXItem
          Tag = -2
          Caption = 'Page width'
          Hint = 'Fit page width into window'
          OnClick = Fitto1Click
        end
        object SpTBXSeparatorItem4: TSpTBXSeparatorItem
        end
        object tbi25: TSpTBXItem
          Tag = 25
          Caption = '25%'
          Hint = '25% zoom'
          OnClick = Fitto1Click
        end
        object tbi50: TSpTBXItem
          Tag = 50
          Caption = '50%'
          Hint = '50% zoom'
          OnClick = Fitto1Click
        end
        object tbi100: TSpTBXItem
          Tag = 100
          Caption = '100%'
          Hint = '100% zoom'
          OnClick = Fitto1Click
        end
        object tbi200: TSpTBXItem
          Tag = 200
          Caption = '200%'
          Hint = '200% zoom'
          OnClick = Fitto1Click
        end
        object tbi400: TSpTBXItem
          Tag = 400
          Caption = '400%'
          Hint = '400% zoom'
          OnClick = Fitto1Click
        end
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object tbiPrint: TSpTBXItem
        Action = PrintCmd
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object tbiClose: TSpTBXItem
        Action = CloseCmd
      end
    end
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 364
    Width = 490
    Height = 10
    object LeftStatusLabel: TSpTBXLabelItem
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      CustomWidth = 455
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object RightStatusLabel: TSpTBXLabelItem
    end
  end
  object ActionList: TActionList
    Images = vilPrintPreview
    Left = 302
    Top = 175
    object FirstCmd: TAction
      Caption = 'First'
      Hint = 'First|Go to first page'
      ImageIndex = 0
      ImageName = 'First'
      ShortCut = 32838
      OnExecute = FirstCmdExecute
      OnHint = ProcessHint
    end
    object PrevCmd: TAction
      Caption = 'Previous'
      Hint = 'Previous|Go to previous page'
      ImageIndex = 1
      ImageName = 'Previous'
      ShortCut = 32848
      OnExecute = PrevCmdExecute
      OnHint = ProcessHint
    end
    object NextCmd: TAction
      Caption = 'Next'
      Hint = 'Next|Go to next page'
      ImageIndex = 2
      ImageName = 'Next'
      ShortCut = 32846
      OnExecute = NextCmdExecute
      OnHint = ProcessHint
    end
    object LastCmd: TAction
      Caption = 'Last'
      Hint = 'Last|Go to last page'
      ImageIndex = 3
      ImageName = 'Last'
      ShortCut = 32844
      OnExecute = LastCmdExecute
      OnHint = ProcessHint
    end
    object ZoomCmd: TAction
      Caption = 'Zoom'
      Hint = 'Zoom|Zoom In/Out'
      ImageIndex = 4
      ImageName = 'ZoomIn'
      ShortCut = 32858
      OnExecute = ZoomCmdExecute
      OnHint = ProcessHint
    end
    object PrintCmd: TAction
      Caption = 'Print'
      Hint = 'Print|Print the document'
      ImageIndex = 5
      ImageName = 'Print'
      ShortCut = 16464
      OnExecute = PrintCmdExecute
      OnHint = ProcessHint
    end
    object CloseCmd: TAction
      Caption = 'Close'
      Hint = 'Close|Close Print Preview'
      ImageIndex = 6
      ImageName = 'Exit'
      ShortCut = 32835
      OnExecute = CloseCmdExecute
      OnHint = ProcessHint
    end
  end
  object vilPrintPreview: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 1
        CollectionName = 'PrintPreview\First'
        Disabled = False
        Name = 'First'
      end
      item
        CollectionIndex = 4
        CollectionName = 'PrintPreview\Previous'
        Disabled = False
        Name = 'Previous'
      end
      item
        CollectionIndex = 3
        CollectionName = 'PrintPreview\Next'
        Disabled = False
        Name = 'Next'
      end
      item
        CollectionIndex = 2
        CollectionName = 'PrintPreview\Last'
        Disabled = False
        Name = 'Last'
      end
      item
        CollectionIndex = 6
        CollectionName = 'PrintPreview\ZoomIn'
        Disabled = False
        Name = 'ZoomIn'
      end
      item
        CollectionIndex = 5
        CollectionName = 'PrintPreview\Print'
        Disabled = False
        Name = 'Print'
      end
      item
        CollectionIndex = 0
        CollectionName = 'PrintPreview\Exit'
        Disabled = False
        Name = 'Exit'
      end>
    ImageCollection = icPrintPreview
    PreserveItems = True
    Left = 232
    Top = 176
  end
  object icPrintPreview: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'PrintPreview\Exit'
        SVGText = 
          '<svg enable-background="new 0 0 32 32" viewBox="0 0 32 32" xmlns' +
          '="http://www.w3.org/2000/svg"><path d="m23.5 10-2.1 2.1 2.4 2.4h' +
          '-12.3v3h12.3l-2.4 2.4 2.1 2.1 6-6zm-18-4.5h10.5v-3h-10.5c-1.7 0-' +
          '3 1.3-3 3v21c0 1.7 1.3 3 3 3h10.5v-3h-10.5z"/></svg>'
      end
      item
        IconName = 'PrintPreview\First'
        SVGText = 
          '<svg enable-background="new 0 0 32 32" viewBox="0 0 32 32" xmlns' +
          '="http://www.w3.org/2000/svg"><path d="m27.4 24.4-8.5-8.4 8.4-8.' +
          '4-2.5-2.6-11 11 11 11zm-19.1 2.6h-3.7v-22h3.7z"/></svg>'
      end
      item
        IconName = 'PrintPreview\Last'
        SVGText = 
          '<svg enable-background="new 0 0 32 32" viewBox="0 0 32 32" xmlns' +
          '="http://www.w3.org/2000/svg"><path d="m7.2 27 11-11-11-11-2.6 2' +
          '.6 8.4 8.4-8.4 8.4zm16.5-22h3.7v22h-3.7z"/></svg>'
      end
      item
        IconName = 'PrintPreview\Next'
        SVGText = 
          '<svg viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg"><pat' +
          'h d="m4 16c0-6.7 5.4-12 12-12s12 5.4 12 12-5.4 12-12 12-12-5.4-1' +
          '2-12m-3 0c0 8.2 6.7 15 15 15s15-6.7 15-15-6.7-15-15-15-15 6.7-15' +
          ' 15z"/><path d="m14 19.9v-7.8l6.1 3.9zm10.4-3.8-12.7-8.1v16.1z"/' +
          '></svg>'
      end
      item
        IconName = 'PrintPreview\Previous'
        SVGText = 
          '<svg enable-background="new 0 0 32 32" viewBox="0 0 32 32" xmlns' +
          '="http://www.w3.org/2000/svg"><path d="m28 16c0 6.6-5.4 12-12 12' +
          's-12-5.4-12-12 5.4-12 12-12 12 5.3 12 12m3 0c0-8.3-6.7-15-15-15s' +
          '-15 6.7-15 15 6.7 15 15 15 15-6.8 15-15z"/><path d="m18 19.9-6.1' +
          '-3.9 6.1-3.9zm2.3 4.2v-16.1l-12.7 8.1z"/></svg>'
      end
      item
        IconName = 'PrintPreview\Print'
        SVGText = 
          '<svg enable-background="new 0 0 32 32" viewBox="0 0 32 32" xmlns' +
          '="http://www.w3.org/2000/svg"><path d="m26.5 10h-1.5v-7.5h-18v7.' +
          '5h-1.5c-2.5 0-4.5 1.9-4.5 4.5v9h6v6h18v-6h6v-9c0-2.6-2.1-4.5-4.5' +
          '-4.5zm-16.5-4.5h12v4.5h-12zm12 18v3h-12v-6h12zm3-3v-3h-18v3h-3v-' +
          '6c0-.8.8-1.5 1.5-1.5h21c.9 0 1.5.8 1.5 1.5v6z"/><circle cx="25" ' +
          'cy="15.2" r="1.5"/></svg>'
      end
      item
        IconName = 'PrintPreview\ZoomIn'
        SVGText = 
          '<svg viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg"><pat' +
          'h d="m2.8 26.9 2.3 2.3 7.5-7.5v-1.2l.5-.4c1.6 1.5 3.9 2.4 6.3 2.' +
          '4 5.4 0 9.8-4.3 9.8-9.8s-4.3-9.8-9.8-9.8-9.8 4.3-9.8 9.7c0 2.4.9' +
          ' 4.7 2.4 6.3l-.4.5h-1.2zm9.8-14.3c0-3.8 3-6.8 6.8-6.8s6.8 3 6.8 ' +
          '6.8-3 6.8-6.8 6.8-6.8-3-6.8-6.8zm6-.7h-3v1.5h3v3h1.5v-3h3v-1.5h-' +
          '3v-3h-1.5z"/></svg>'
      end>
    Left = 152
    Top = 176
  end
end
