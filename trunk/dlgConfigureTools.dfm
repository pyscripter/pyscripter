object ConfigureTools: TConfigureTools
  Left = 342
  Top = 215
  HelpContext = 710
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Configure Tools'
  ClientHeight = 267
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    270
    267)
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsList: TListBox
    Left = 8
    Top = 8
    Width = 165
    Height = 251
    Anchors = [akLeft, akTop, akRight, akBottom]
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 5
    OnClick = ToolsListClick
    OnDblClick = ModifyBtnClick
    OnDragDrop = ToolsListDragDrop
    OnDragOver = ToolsListDragOver
  end
  object AddBtn: TSpTBXButton
    Left = 184
    Top = 12
    Width = 75
    Height = 25
    Caption = '&Add...'
    Anchors = [akTop, akRight]
    TabOrder = 0
    OnClick = AddBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
  object RemoveBtn: TSpTBXButton
    Left = 184
    Top = 76
    Width = 75
    Height = 25
    Caption = '&Remove'
    Anchors = [akTop, akRight]
    TabOrder = 2
    OnClick = RemoveBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
  object ModifyBtn: TSpTBXButton
    Left = 184
    Top = 44
    Width = 75
    Height = 25
    Caption = '&Modify..'
    Anchors = [akTop, akRight]
    TabOrder = 1
    OnClick = ModifyBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
  object OKBtn: TSpTBXButton
    Left = 184
    Top = 206
    Width = 75
    Height = 25
    Caption = 'OK'
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Default = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
    ModalResult = 1
  end
  object CancelBtn: TSpTBXButton
    Left = 184
    Top = 238
    Width = 75
    Height = 25
    Caption = 'Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 4
    Cancel = True
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
    ModalResult = 2
  end
  object MoveUpBtn: TSpTBXButton
    Left = 184
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Move &Up'
    Anchors = [akTop, akRight]
    TabOrder = 6
    OnClick = MoveUpBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
  object MoveDownBtn: TSpTBXButton
    Left = 184
    Top = 140
    Width = 75
    Height = 25
    Caption = 'Move &Down'
    Anchors = [akTop, akRight]
    TabOrder = 7
    OnClick = MoveDownBtnClick
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'Tahoma'
    LinkFont.Style = [fsUnderline]
  end
end
