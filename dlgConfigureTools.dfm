inherited ConfigureTools: TConfigureTools
  Left = 342
  Top = 215
  HelpContext = 710
  BorderIcons = [biSystemMenu]
  Caption = 'Configure Tools'
  ClientHeight = 267
  ClientWidth = 301
  Font.Name = 'MS Shell Dlg 2'
  OldCreateOrder = True
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 307
  ExplicitHeight = 293
  DesignSize = (
    301
    267)
  PixelsPerInch = 96
  TextHeight = 13
  object AddBtn: TSpTBXButton
    Left = 200
    Top = 12
    Width = 90
    Height = 25
    Caption = '&Add...'
    Anchors = [akTop, akRight]
    TabOrder = 0
    OnClick = AddBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object RemoveBtn: TSpTBXButton
    Left = 200
    Top = 76
    Width = 90
    Height = 25
    Caption = '&Remove'
    Anchors = [akTop, akRight]
    TabOrder = 2
    OnClick = RemoveBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object ModifyBtn: TSpTBXButton
    Left = 200
    Top = 44
    Width = 90
    Height = 25
    Caption = '&Modify..'
    Anchors = [akTop, akRight]
    TabOrder = 1
    OnClick = ModifyBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object OKBtn: TSpTBXButton
    Left = 200
    Top = 206
    Width = 90
    Height = 25
    Caption = '&OK'
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
  end
  object CancelBtn: TSpTBXButton
    Left = 200
    Top = 237
    Width = 90
    Height = 25
    Caption = '&Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 4
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object MoveUpBtn: TSpTBXButton
    Left = 200
    Top = 108
    Width = 90
    Height = 25
    Caption = '&Up'
    Anchors = [akTop, akRight]
    TabOrder = 6
    OnClick = MoveUpBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object MoveDownBtn: TSpTBXButton
    Left = 200
    Top = 140
    Width = 90
    Height = 25
    Caption = '&Down'
    Anchors = [akTop, akRight]
    TabOrder = 7
    OnClick = MoveDownBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = [fsUnderline]
  end
  object ToolsList: TSpTBXListBox
    Left = 8
    Top = 8
    Width = 181
    Height = 251
    Style = lbStandard
    Anchors = [akLeft, akTop, akRight, akBottom]
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 5
    OnClick = ToolsListClick
    OnDblClick = ModifyBtnClick
    OnDragDrop = ToolsListDragDrop
    OnDragOver = ToolsListDragOver
    ThemeType = thtWindows
  end
end
