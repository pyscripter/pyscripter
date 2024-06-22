object SuggestWindow: TSuggestWindow
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  BorderWidth = 1
  ClientHeight = 327
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  StyleElements = [seClient]
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object seSuggest: TSynEdit
    Left = 0
    Top = 0
    Width = 622
    Height = 305
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.Visible = False
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    RightEdge = 0
    SelectedColor.Alpha = 0.400000005960464500
  end
  object SpTBXDock: TSpTBXDock
    Left = 0
    Top = 305
    Width = 622
    Height = 22
    Position = dpBottom
    object SpTBXToolbar: TSpTBXToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      DragHandleStyle = dhNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Segoe UI'
      Font.Style = []
      FullSize = True
      Images = vilImages
      ParentFont = False
      ProcessShortCuts = True
      ShrinkMode = tbsmNone
      Stretch = True
      SystemFont = False
      TabOrder = 0
      Caption = 'SpTBXToolbar'
      Customizable = False
      object spiAccept: TSpTBXItem
        Caption = 'Accept (Tab)'
        DisplayMode = nbdmImageAndText
        ImageIndex = 1
        ImageName = 'Check'
        ShortCut = 9
        OnClick = spiAcceptClick
      end
      object spiAcceptWord: TSpTBXItem
        Caption = 'Accept Word (Ctrl+Right)'
        Hint = 'Accept the first suggested word'
        ShortCut = 16423
        OnClick = spiAcceptWordClick
      end
      object spiAcceptLine: TSpTBXItem
        Caption = 'Accept Line (Ctrl+Enter)'
        Hint = 'Accept the first suggested line'
        ShortCut = 16397
        OnClick = spiAcceptLineClick
      end
      object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
        CustomWidth = 261
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object spiCancel: TSpTBXItem
        Caption = 'Cancel (Esc)'
        Hint = 'Close the form'
        DisplayMode = nbdmImageAndText
        ImageIndex = 0
        ImageName = 'Cancel'
        ShortCut = 27
        OnClick = spiCancelClick
      end
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 12
        CollectionName = 'Cancel'
        Name = 'Cancel'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Check'
        Name = 'Check'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 10
    Height = 10
    Left = 16
    Top = 240
  end
end
