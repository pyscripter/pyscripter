inherited ToDoWindow: TToDoWindow
  Left = 377
  Top = 226
  HelpContext = 465
  Caption = 'To Do List'
  ClientHeight = 293
  ClientWidth = 492
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
    object mnSeparator1: TSpTBXSeparatorItem
    end
    object mnCopyAll: TSpTBXItem
      Action = actEditCopy
    end
    object mnSeparator2: TSpTBXSeparatorItem
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
        CollectionIndex = 80
        CollectionName = 'Print'
        Name = 'Print'
      end
      item
        CollectionIndex = 26
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 113
        CollectionName = 'Setup'
        Name = 'Setup'
      end
      item
        CollectionIndex = 61
        CollectionName = 'GoToLine'
        Name = 'GoToLine'
      end
      item
        CollectionIndex = 62
        CollectionName = 'Help'
        Name = 'Help'
      end
      item
        CollectionIndex = 98
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
