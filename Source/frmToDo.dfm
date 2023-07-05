inherited ToDoWindow: TToDoWindow
  Left = 377
  Top = 226
  HelpContext = 465
  Caption = 'To Do List'
  ClientHeight = 293
  ClientWidth = 492
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000005F4734FF664C38FF644A36FF684F3BFF77614FFF6B523FFF644A
    36FF644A36FF644A36FF644A36FF644A36FF0000000000000000000000000000
    0000BCA798FFF7EEE9FFE6D8D1FFE0CDC4FFD8C4BDFF8F8CB1FFD5BDB4FFD8B7
    A7FFD5B1A0FFD4AF99FFD0AA94FFC89F88FF644A36FF00000000000000000000
    0000BEA99AFFF9F2EEFFF7EEE9FFF5E9E4FF3050BEFF1639B3FF6879C4FFE4D0
    C8FFE9D0C3FFE7CCBEFFE3C5B6FFD3AA95FF644A36FF00000000000000000000
    0000BEA99AFFF9F2EEFFD4D2E1FF284CC0FF2854EDFF234BE2FF1238B5FF9B97
    C2FFEAD3C6FFE8CFC2FFE0C4B5FFD3AB96FF644A36FF00000000000000000000
    0000C1AC9DFFFAF5F2FF1E43BFFF2A57F4FF6183FBFF5376F6FF3D63EAFF203F
    B3FFD0C2C6FFEBD6CAFFE3CBBEFFD0AF9BFF644A36FF00000000000000000000
    0000C5AFA0FFFCF8F6FF86A1FBFF7895FBFF7A8FF5FFD6D2E4FF7F95F1FF4262
    E2FF3E55AFFFE9D5CDFFECD7CDFFD3B5A3FF674D39FF00000000000000000000
    0000C8B3A3FFFDFBFAFFE6E6F8FFBEC7F6FFF4EDECFFF6ECE7FFE2DBE2FF7B91
    F0FF2E54DDFF5666AEFFEEDED6FFDCC5B6FF745A46FF00000000000000000000
    0000CBB6A6FFFEFDFDFFFDFBFAFFFBF8F5FFFAF4F1FFF8F0ECFFF6ECE6FFE9E0
    E2FF738BF1FF264CD7FF8D92BAFFE3CFC4FF856D5BFF00000000000000000000
    0000CFB9A9FFFFFFFFFFFEFDFDFFFDFAF9FFFBF7F5FFFAF4F0FFF6EEEAFFEAE3
    E1FFF0E8E3FF7D92ECFF244AD7FF9896B8FFA08D7FFF00000000000000000000
    0000D2BCACFFFFFFFFFFFFFEFDFF7E9FA9FF618894FF638694FF617B89FF5D77
    84FF6C818EFF95A2A9FF8AA0EAFF284ED6FFA8978AFF00000000000000000000
    0000D4BEAEFFFFFFFFFFFFFFFEFF87A9B6FF94D7E1FF97E5F1FF7ED7E8FF62C5
    DDFF5199B0FF6F828BFFF1E4DEFFE3D4CDFFA59688FF00000000000000000000
    0000CDB8A8FFFFFFFFFFFFFFFFFFF6FBFCFF80A7B3FFA2ABA0FF87776DFF78C5
    D6FF4C6C7CFFF7EEE9FFEDE3DCFFF0E3DCFF836F5FFF00000000000000000000
    000000000000C7B2A4FFD4BEAFFFD0BDAFFF72A5B4FFA4E7EFFF9BE7F3FF8AD3
    E0FF466676FFC0AB9CFFBDA999FFBCA797FF0000000000000000000000000000
    0000000000000000000000000000000000000000000086B3C3FF7AA3B3FF6E92
    A3FF000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 492
    Height = 293
    inherited FGPanel: TPanel
      Width = 488
      Height = 289
      object TBXDock1: TSpTBXDock
        Left = 0
        Top = 0
        Width = 488
        Height = 30
        AllowDrag = False
        DoubleBuffered = True
        object Toolbar: TSpTBXToolbar
          Left = 0
          Top = 0
          DockPos = 0
          FullSize = True
          Images = vicImages
          TabOrder = 0
          Customizable = False
          object tbiRefresh: TSpTBXItem
            Action = actFileRefresh
          end
          object tbiAbort: TSpTBXItem
            Action = actFileAbort
          end
          object TBXSeparatorItem1: TSpTBXSeparatorItem
          end
          object tbiGoTo: TSpTBXItem
            Action = actEditGoto
          end
          object TBXSeparatorItem2: TSpTBXSeparatorItem
          end
          object tbiPrint: TSpTBXItem
            Action = actFilePrint
          end
          object TBXSeparatorItem3: TSpTBXSeparatorItem
          end
          object tbiOptions: TSpTBXItem
            Action = actOptionsConfigure
          end
          object TBXSeparatorItem5: TSpTBXSeparatorItem
          end
          object tbiHelp: TSpTBXItem
            Action = actHelpHelp
          end
        end
      end
      object ToDoView: TVirtualStringTree
        Left = 0
        Top = 30
        Width = 488
        Height = 259
        Align = alClient
        Alignment = taRightJustify
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Header.AutoSizeIndex = 1
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoHotTrack, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
        HintMode = hmTooltip
        Images = ToDoImages
        PopupMenu = PopupMenu
        TabOrder = 1
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnChange = ToDoViewChange
        OnCompareNodes = ToDoViewCompareNodes
        OnDblClick = actEditGotoExecute
        OnGetText = ToDoViewGetText
        OnGetImageIndex = ToDoViewGetImageIndex
        OnHeaderClick = ToDoViewHeaderClick
        OnInitNode = ToDoViewInitNode
        OnKeyPress = TodoViewKeyPress
        OnShortenString = ToDoViewShortenString
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Alignment = taCenter
            Layout = blGlyphRight
            Margin = 0
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
            Position = 0
            Spacing = 0
            Text = '!'
            Width = 25
          end
          item
            Layout = blGlyphRight
            MinWidth = 100
            Position = 1
            Text = 'Description'
            Width = 203
          end
          item
            Layout = blGlyphRight
            MinWidth = 50
            Position = 2
            Text = 'File Name'
            Width = 200
          end
          item
            Alignment = taRightJustify
            Layout = blGlyphRight
            MinWidth = 20
            Position = 3
            Text = 'Line'
            Width = 60
          end>
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 456
    Top = 8
  end
  object PopupMenu: TSpTBXPopupMenu
    Images = vicImages
    Left = 128
    Top = 121
    object mnGoto: TSpTBXItem
      Action = actEditGoto
    end
    object mnRefresh: TSpTBXItem
      Action = actFileRefresh
    end
    object N1: TSpTBXSeparatorItem
    end
    object mnCopyAll: TSpTBXItem
      Action = actEditCopy
    end
    object N2: TSpTBXSeparatorItem
    end
    object mnPrint: TSpTBXItem
      Action = actFilePrint
    end
    object mnOptions: TSpTBXItem
      Action = actOptionsConfigure
    end
    object TBXSeparatorItem6: TSpTBXSeparatorItem
    end
    object mnHelp: TSpTBXItem
      Action = actHelpHelp
    end
  end
  object Actions: TActionList
    Images = vicImages
    Left = 40
    Top = 120
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh to do items'
      ImageIndex = 5
      ImageName = 'Refresh'
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actEditGoto: TAction
      Category = 'Edit'
      Caption = '&Goto'
      Hint = 'Goto source line'
      ImageIndex = 3
      ImageName = 'GoToLine'
      OnExecute = actEditGotoExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print to do items'
      ImageIndex = 0
      ImageName = 'Print'
      ShortCut = 16464
      OnExecute = actFilePrintExecute
      OnUpdate = actFilePrintUpdate
    end
    object actOptionsConfigure: TAction
      Category = 'Options'
      Caption = '&Options...'
      Hint = 'Options...'
      ImageIndex = 2
      ImageName = 'Setup'
      OnExecute = actOptionsConfigureExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 4
      ImageName = 'Help'
      OnExecute = actHelpHelpExecute
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy All'
      Hint = 'Copy all'
      ImageIndex = 1
      ImageName = 'Copy'
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actFileAbort: TAction
      Category = 'File'
      Caption = '&Abort'
      Enabled = False
      Hint = 'Abort search for todo items'
      ImageIndex = 6
      ImageName = 'Abort'
      ShortCut = 16449
      OnExecute = actFileAbortExecute
    end
  end
  object ToDoImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 1
        CollectionName = 'ToDo\Exclamation'
        Name = 'Exclamation'
      end
      item
        CollectionName = 'ToDo\MediumPriority'
        Name = 'MediumPriority'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ToDo\LowPriority'
        Name = 'LowPriority'
      end
      item
        CollectionIndex = 0
        CollectionName = 'ToDo\Done'
        Name = 'Done'
      end>
    ImageCollection = icTodo
    PreserveItems = True
    Left = 128
    Top = 64
  end
  object vicImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 70
        CollectionName = 'Print'
        Name = 'Print'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 103
        CollectionName = 'Setup'
        Name = 'Setup'
      end
      item
        CollectionIndex = 51
        CollectionName = 'GoToLine'
        Name = 'GoToLine'
      end
      item
        CollectionIndex = 52
        CollectionName = 'Help'
        Name = 'Help'
      end
      item
        CollectionIndex = 88
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 0
        CollectionName = 'Abort'
        Name = 'Abort'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 201
    Top = 64
  end
  object icTodo: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'ToDo\Done'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10#9'<path d="M11.2,21.8l-6.3-6.3l-2.1,2' +
          '.1l8.4,8.4l18-18L27.1,6L11.2,21.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ToDo\Exclamation'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10#9'<circle cx="16" cy="26.5" r="3"/>'#13#10 +
          #9'<path d="M13,2.5h6v18h-6V2.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ToDo\LowPriority'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M28,17.5l-2.1-2.1l-8.4,8.4V' +
          '2.5h-3v21.3l-8.4-8.4L4,17.5l12,12L28,17.5z"/>'#13#10'</svg>'#13#10
      end>
    ApplyFixedColorToRootOnly = True
    Left = 40
    Top = 64
  end
end
