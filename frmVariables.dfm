inherited VariablesWindow: TVariablesWindow
  Left = 237
  Top = 237
  HelpContext = 480
  Caption = 'Variables'
  ClientHeight = 247
  ClientWidth = 579
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000001111
    11640F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F
    0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F850F0F0F85111111642727
    277BEBEBEBFFE3DCCDFFB59037FFE3DCCDFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
    E7FFD3D3E6FF5959C6FFD3D3E6FFE7E7E7FFE7E7E7FFEBEBEBFF2727277B3131
    3176E5DFD0FFAB7D0EFFD1A334FFB38E35FFDED8C9FFE2E2E2FFE2E2E2FFCFCF
    E1FF3838C0FF5E5EE6FF5757C4FFCFCFE1FFE2E2E2FFEAEAEAFF313131763737
    3773AD7F10FFE1B344FFFFD869FFDDAF40FFB59037FFE2DCCCFFE6E6E6FF3A3A
    C2FF6E6EF6FF9393FFFF6A6AF2FF5959C6FFD2D2E5FFEDEDEDFF373737733C3C
    3C71EBE4D5FFC19324FFFFD869FFFFD869FFFFE273FFAB7D0EFFEBEBEBFFD6D6
    E9FF4E4ED6FF9393FFFF9393FFFF9D9DFFFF3838C0FFF0F0F0FF3C3C3C714343
    436EF3F3F3FFEAE3D4FFD0A233FFFFE273FFAA7C0DFFEAE3D4FFEFEFEFFFEFEF
    EFFFDADAEDFF5D5DE5FF9D9DFFFF3737BFFFDADAEDFFF3F3F3FF4343436E4949
    496CF7F7F7FFF4F4F4FFEEE8D8FFAA7C0DFFEEE8D8FFF4F4F4FFD3E5E9FF2F81
    99FFD3E5E9FFDEDEF1FF3737BFFFDEDEF1FFF4F4F4FFF7F7F7FF4949496C4C4C
    4C6AFAFAFAFFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFD7E8ECFF006685FF0D8C
    ABFF2F8099FFD7E8ECFFF8F8F8FFF8F8F8FFF8F8F8FFFAFAFAFF4C4C4C6A5050
    5068FDFDFDFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF006887FF1D9CBBFF42C1
    E0FF1998B7FF31829BFFDAEBF0FFFCFCFCFFFCFCFCFFFDFDFDFF505050685454
    5467FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDDEEF2FF007C9BFF42C1
    E0FF42C1E0FF4CCBEAFF006685FFFFFFFFFFFFFFFFFFFFFFFFFF54545467D692
    3DE1EEB266FFEDB165FFEBAF63FFE9AD61FFE6AA5EFFE4A85CFFC3A063FF0C8B
    AAFF4CCBEAFF006584FFB9975AFFD3974BFFD19549FFCF9347FFB06B14E0D391
    3FCDF4C375FDFED287FFFDCE85FFFACA83FFF8C57FFFF6C17BFFF4BD74FFD2B3
    73FF006584FFD3B65BFFF7C23EFFF9C82BFFFBCD19FFF2C020FDD3913FCDC28A
    455CD3953FCDD9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D998
    3EE0D9983EE0D9983EE0D9983EE0D9983EE0D9983EE0D3953FCDC28A455C0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  ExplicitWidth = 595
  ExplicitHeight = 286
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 575
    Height = 243
    ExplicitLeft = 2
    ExplicitTop = 2
    ExplicitWidth = 575
    ExplicitHeight = 243
    object VariablesTree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 195
      Height = 243
      Align = alClient
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Header.AutoSizeIndex = 2
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoHotTrack, hoOwnerDraw, hoVisible]
      Header.ParentFont = True
      Header.PopupMenu = VTHeaderPopupMenu
      HintMode = hmTooltip
      Images = CommandsDataModule.CodeImages
      IncrementalSearch = isVisibleOnly
      TabOrder = 0
      TreeOptions.AnimationOptions = [toAnimatedToggle]
      TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
      TreeOptions.StringOptions = [toAutoAcceptEditChange]
      OnChange = VariablesTreeChange
      OnGetText = VariablesTreeGetText
      OnPaintText = VariablesTreePaintText
      OnGetImageIndex = VariablesTreeGetImageIndex
      OnInitChildren = VariablesTreeInitChildren
      OnInitNode = VariablesTreeInitNode
      Columns = <
        item
          Position = 0
          Width = 160
          WideText = 'Name'
        end
        item
          Position = 1
          Width = 100
          WideText = 'Type'
        end
        item
          Position = 2
          Width = 10
          WideText = 'Value'
        end>
    end
    object DocPanel: TSpTBXPageScroller
      Left = 200
      Top = 0
      Width = 375
      Height = 243
      Align = alRight
      Color = clBtnFace
      ParentColor = False
      TabOrder = 1
      object HTMLLabel: TJvLinkLabel
        Left = 0
        Top = 0
        Width = 375
        Caption = '<b>NameSpace'
        Text.Strings = (
          '<b>NameSpace')
        MarginWidth = 2
        MarginHeight = 2
        Align = alTop
        Color = clBtnFace
        ParentColor = False
      end
    end
    object SpTBXSplitter: TSpTBXSplitter
      Left = 195
      Top = 0
      Height = 243
      Cursor = crSizeWE
      Align = alRight
      Color = clNone
      ParentColor = False
      MinSize = 3
    end
  end
  inherited DockClient: TJvDockClient
    LeftDock = False
    RightDock = False
    Left = 24
    Top = 26
  end
  object VTHeaderPopupMenu: TVTHeaderPopupMenu
    Left = 20
    Top = 75
  end
end
