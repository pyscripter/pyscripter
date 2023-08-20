inherited BreakPointsWindow: TBreakPointsWindow
  Left = 491
  Top = 381
  HelpContext = 495
  Caption = 'Breakpoints'
  ClientHeight = 244
  ClientWidth = 379
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    000000006D0E00006369000063AE000061C7000063AE0000636900006D0E0000
    0000000000000000000000000000000000000000000000000000000000000000
    7F0E00007C9A09099BDF0E0EC3F61111CFFD0E0EC2F604049BDF00007C9A0000
    7F0E000000000000000000000000000000000000000000000000000000000606
    5BA40D0D95F01717C7FF1111C5FF1111C5FF1111C5FF1111C4FF070790F00505
    53B70F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F85111111640707
    7BD53434C1FF1111B2FF1111B2FF1111B2FF1111B2FF1111B2FF1919B0FF4949
    AAFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFEBEBEBFF2727277B0404
    89E14949D0FF2727B3FF1616A6FF1111A2FF1111A2FF1111A2FF1C1CAAFF3131
    A5FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFEAEAEAFF313131760808
    88D35050D6FF3A3AC2FF3838C0FF3131B9FF2A2AB2FF2929B1FF3939BFFF4949
    B2FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FFEDEDEDFF373737731616
    7AAB4444C8FF5A5AE2FF4F4FD7FF4F4FD7FF4F4FD7FF5757DFFF3F3FC3FF8A8A
    CCFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFEBEBEBFFF0F0F0FF3C3C3C713A3A
    4F766060C3FF4646CCFF6262EAFF6A6AF0FF6161E9FF4545CCFF5E5EC2FFE1E1
    EAFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFF3F3F3FF4343436E4949
    496CE9E9F2FF8F8FD5FF4D4DC0FF3535B9FF4D4DC0FF8F8FD5FFE6E6EFFFF4F4
    F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF7F7F7FF4949496C4C4C
    4C6AFAFAFAFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8
    F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFFAFAFAFF4C4C4C6A5050
    5068FDFDFDFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
    FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFDFDFDFF505050685454
    5467FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF54545467D692
    3DE1EEB266FFEDB165FFEBAF63FFE9AD61FFE6AA5EFFE4A85CFFE1A559FFDEA2
    56FFDB9F53FFD89C50FFD69A4EFFD3974BFFD19549FFCF9347FFB06B14E0D391
    3FCDF4C375FDFED287FFFDCE85FFFACA83FFF8C57FFFF6C17BFFF4BD74FFF3BB
    6CFFF3BC60FFF4BE50FFF7C23EFFF9C82BFFFBCD19FFF2C020FDD3913FCDC28A
    455CD3953FCDD9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D998
    3EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D3953FCDC28A455C0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  TextHeight = 15
  inherited BGPanel: TPanel
    Width = 379
    Height = 244
    inherited FGPanel: TPanel
      Width = 375
      Height = 240
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 375
        Height = 240
        Align = alClient
        TabOrder = 0
        object BreakPointsView: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 373
          Height = 238
          Align = alClient
          Alignment = taRightJustify
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = 2
          Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
          HintMode = hmTooltip
          PopupMenu = TBXPopupMenu
          TabOrder = 0
          TreeOptions.AnimationOptions = [toAnimatedToggle]
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
          TreeOptions.MiscOptions = [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toHideSelection, toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnChecked = BreakPointsViewChecked
          OnDblClick = BreakPointLVDblClick
          OnGetText = BreakPointsViewGetText
          OnInitNode = BreakPointsViewInitNode
          OnKeyDown = BreakPointsViewKeyDown
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Position = 0
              Text = 'File Name'
              Width = 200
            end
            item
              Alignment = taRightJustify
              Position = 1
              Text = 'Line'
            end
            item
              Position = 2
              Text = 'Condition'
              Width = 123
            end>
        end
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 12
    Top = 18
  end
  object TBXPopupMenu: TSpTBXPopupMenu
    Images = vilImages
    OnPopup = TBXPopupMenuPopup
    Left = 21
    Top = 66
    object mnSetCondition: TSpTBXItem
      Caption = 'Set &Condition...'
      OnClick = mnSetConditionClick
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object mnClear: TSpTBXItem
      Caption = 'C&lear'
      Hint = 'Clear|Clear selected breakpoint'
      OnClick = mnClearClick
    end
    object Breakpoints1: TSpTBXItem
      Action = PyIDEMainForm.actClearAllBreakpoints
      ImageIndex = 1
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object mnCopyToClipboard: TSpTBXItem
      Caption = 'Co&py to Clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 0
      ImageName = 'Copy'
      OnClick = mnCopyToClipboardClick
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 16
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 7
        CollectionName = 'BreakpointsRemove'
        Name = 'BreakpointsRemove'
      end>
    ImageCollection = ResourcesDataModule.icSVGImages
    PreserveItems = True
    Width = 20
    Height = 20
    Left = 16
    Top = 120
  end
end
