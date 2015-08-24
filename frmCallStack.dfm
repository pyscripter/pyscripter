inherited CallStackWindow: TCallStackWindow
  Left = 569
  Top = 397
  HelpContext = 470
  Caption = 'Call Stack'
  ClientHeight = 168
  ClientWidth = 381
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000001111
    11640F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F
    0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F85111111642727
    277BEBEBEBFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
    E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFEBEBEBFF2727277B3131
    3176EAEAEAFFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FF5A9266FF2D7238FF2D72
    38FF2D7238FF2D7238FF2D7238FF2D7238FF5A9266FFEAEAEAFF313131763737
    3773EDEDEDFFE6E6E6FFE6E6E6FFE6E6E6FFE6E6E6FF2E8A4BFF50BD87FF40B2
    79FF27A868FF29AD6BFF29B06DFF33B976FF2E8A4BFFEDEDEDFF373737733C3C
    3C71F0F0F0FFEBEBEBFFEBEBEBFF5E956AFF2F733AFF12853AFF098B3CFF098B
    3CFF098B3CFF098B3CFF129246FF2FA460FF5EB582FFF0F0F0FF3C3C3C714343
    436EF3F3F3FFEFEFEFFFEFEFEFFF2F8B4DFF57C48EFF50BD87FF27A868FF29AD
    6BFF29B06DFF33B976FF2F8B4DFFEFEFEFFFEFEFEFFFF3F3F3FF4343436E4949
    496CF7F7F7FF61996DFF30753CFF13863BFF098C3CFF098C3CFF098C3CFF098C
    3CFF139346FF30A662FF61B986FFF4F4F4FFF4F4F4FFF7F7F7FF4949496C4C4C
    4C6AFAFAFAFF318D4FFF57C48EFF50BD87FF40B279FF27A868FF29B06DFF33B9
    76FF318D4FFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFFAFAFAFF4C4C4C6A5050
    5068FDFDFDFF64BC89FF32A864FF32A864FF32A864FF32A864FF32A864FF32A8
    64FF64BC89FFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFDFDFDFF505050685454
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
  ExplicitWidth = 397
  ExplicitHeight = 207
  PixelsPerInch = 96
  TextHeight = 13
  inherited BGPanel: TSpTBXPanel
    Width = 381
    Height = 168
    ExplicitWidth = 381
    ExplicitHeight = 168
    inherited FGPanel: TPanel
      Width = 377
      Height = 164
      ExplicitWidth = 377
      ExplicitHeight = 164
      object CallStackView: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 377
        Height = 164
        Align = alClient
        Alignment = taRightJustify
        BevelInner = bvNone
        BevelOuter = bvNone
        BevelKind = bkFlat
        BorderStyle = bsNone
        Color = 2500134
        Colors.FocusedSelectionColor = 3158064
        Colors.FocusedSelectionBorderColor = 855309
        Colors.SelectionRectangleBlendColor = 3158064
        Colors.SelectionRectangleBorderColor = 855309
        Colors.TreeLineColor = clWhite
        Colors.UnfocusedSelectionColor = cl3DDkShadow
        Colors.UnfocusedSelectionBorderColor = 855309
        Header.AutoSizeIndex = 1
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
        Header.ParentFont = True
        HintMode = hmTooltip
        TabOrder = 0
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
        TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnChange = CallStackViewChange
        OnDblClick = CallStackViewDblClick
        OnGetText = CallStackViewGetText
        OnInitNode = CallStackViewInitNode
        Columns = <
          item
            Position = 0
            Width = 100
            WideText = 'Function Name'
          end
          item
            Position = 1
            Width = 227
            WideText = 'File Name'
          end
          item
            Alignment = taRightJustify
            Position = 2
            WideText = 'Line'
          end>
      end
    end
  end
  inherited DockClient: TJvDockClient
    Left = 7
    Top = 25
  end
  object actlCallStack: TActionList
    Left = 12
    Top = 76
    object actPreviousFrame: TAction
      Category = 'Run'
      Caption = 'Previous Frame'
      Enabled = False
      HelpContext = 470
      HelpType = htContext
      Hint = 'Select previous (older) frame'
      ShortCut = 122
      OnExecute = actPreviousFrameExecute
    end
    object actNextFrame: TAction
      Category = 'Run'
      Caption = 'Next Frame'
      Enabled = False
      HelpContext = 470
      HelpType = htContext
      Hint = 'Select next (newer) frame'
      ShortCut = 8314
      OnExecute = actNextFrameExecute
    end
  end
end
