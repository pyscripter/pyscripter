object ConfigureTools: TConfigureTools
  Left = 342
  Top = 215
  HelpContext = 710
  ActiveControl = AddBtn
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
  object AddBtn: TButton
    Left = 184
    Top = 12
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add...'
    TabOrder = 0
    OnClick = AddBtnClick
  end
  object RemoveBtn: TButton
    Left = 184
    Top = 76
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Remove'
    TabOrder = 2
    OnClick = RemoveBtnClick
  end
  object ModifyBtn: TButton
    Left = 184
    Top = 44
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Modify..'
    TabOrder = 1
    OnClick = ModifyBtnClick
  end
  object OKBtn: TButton
    Left = 184
    Top = 206
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelBtn: TButton
    Left = 184
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object MoveUpBtn: TButton
    Left = 184
    Top = 108
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Move &Up'
    TabOrder = 6
    OnClick = MoveUpBtnClick
  end
  object MoveDownBtn: TButton
    Left = 184
    Top = 140
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Move &Down'
    TabOrder = 7
    OnClick = MoveDownBtnClick
  end
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
end
