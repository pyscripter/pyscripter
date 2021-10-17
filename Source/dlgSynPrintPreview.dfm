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
    Top = 30
    Width = 490
    Height = 334
    ScaleMode = pscWholePage
    OnMouseDown = SynEditPrintPreviewMouseDown
    OnPreviewPage = SynEditPrintPreviewPreviewPage
  end
  object ToolbarDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 490
    Height = 30
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
    SizeGrip = False
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
    Width = 20
    Height = 20
    Left = 232
    Top = 176
  end
  object icPrintPreview: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'PrintPreview\Exit'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M23.5,10l-2.1,2.1l2.4,2.4H1' +
          '1.5v3h12.3l-2.4,2.4l2.1,2.1l6-6L23.5,10z M5.5,5.5H16v-3H5.5c-1.7' +
          ',0-3,1.3-3,3v21'#13#10#9#9'c0,1.7,1.3,3,3,3H16v-3H5.5V5.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PrintPreview\First'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M27.4,24.4L18.9,16l8.4-8.4L' +
          '24.8,5l-11,11l11,11L27.4,24.4z M8.3,27H4.6V5h3.7V27z"/>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'PrintPreview\Last'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M7.2,27l11-11L7.2,5L4.6,7.6' +
          'l8.4,8.4l-8.4,8.4L7.2,27z M23.7,5h3.7v22h-3.7V5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PrintPreview\Next'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10' <path d="M4,16C4,9.3,9.4,4,16,4s12,' +
          '5.4,12,12s-5.4,12-12,12S4,22.6,4,16 M1,16c0,8.2,6.7,15,15,15s15-' +
          '6.7,15-15S24.3,1,16,1'#13#10#9#9#9'S1,7.7,1,16L1,16z"/>'#13#10'<path d="M14,19.' +
          '9v-7.8l6.1,3.9L14,19.9 M24.4,16.1L11.7,8v16.1L24.4,16.1z"/>'#13#10'</s' +
          'vg>'#13#10
      end
      item
        IconName = 'PrintPreview\Previous'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M28,16c0,6.6-5.4,12-12,12S4' +
          ',22.6,4,16S9.4,4,16,4S28,9.3,28,16 M31,16c0-8.3-6.7-15-15-15S1,7' +
          '.7,1,16s6.7,15,15,15'#13#10#9#9'S31,24.2,31,16L31,16z"/>'#13#10#9'<path id="Tra' +
          'zado_1725_1_" d="M18,19.9L11.9,16l6.1-3.9V19.9 M20.3,24.1V8L7.6,' +
          '16.1L20.3,24.1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PrintPreview\Print'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M26.5,10H25V2.5H7V10H5.5C3,' +
          '10,1,11.9,1,14.5v9h6v6h18v-6h6v-9C31,11.9,28.9,10,26.5,10z M10,5' +
          '.5h12V10H10V5.5z M22,23.5v3'#13#10#9#9'H10v-6h12V23.5z M25,20.5v-3H7v3H4' +
          'v-6C4,13.7,4.8,13,5.5,13h21c0.9,0,1.5,0.8,1.5,1.5v6H25z"/>'#13#10#9'<ci' +
          'rcle cx="25" cy="15.2" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PrintPreview\ZoomIn'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10#9'<path d="M2.8,26.9l2.3,2.3l7.5-7.5v' +
          '-1.2l0.5-0.4c1.6,1.5,3.9,2.4,6.3,2.4c5.4,0,9.8-4.3,9.8-9.8s-4.3-' +
          '9.8-9.8-9.8s-9.8,4.3-9.8,9.7'#13#10#9#9'c0,2.4,0.9,4.7,2.4,6.3l-0.4,0.5h' +
          '-1.2L2.8,26.9z M12.6,12.6c0-3.8,3-6.8,6.8-6.8s6.8,3,6.8,6.8s-3,6' +
          '.8-6.8,6.8S12.6,16.4,12.6,12.6'#13#10#9#9'z M18.6,11.9h-3v1.5h3v3h1.5v-3' +
          'h3v-1.5h-3v-3h-1.5V11.9z"/>'#13#10'</svg>'#13#10
      end>
    ApplyFixedColorToRootOnly = True
    Left = 152
    Top = 176
  end
end
