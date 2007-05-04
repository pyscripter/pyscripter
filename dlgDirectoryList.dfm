object JvDirectoryListDialog: TJvDirectoryListDialog
  Left = 206
  Top = 99
  BorderIcons = [biSystemMenu]
  Caption = 'Directory list'
  ClientHeight = 194
  ClientWidth = 422
  Color = clBtnFace
  Constraints.MinHeight = 208
  Constraints.MinWidth = 358
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    422
    194)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoryList: TListBox
    Left = 8
    Top = 8
    Width = 322
    Height = 178
    Anchors = [akLeft, akTop, akRight, akBottom]
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 5
    OnClick = DirectoryListClick
    OnDragDrop = DirectoryListDragDrop
    OnDragOver = DirectoryListDragOver
  end
  object AddBtn: TSpTBXButton
    Left = 336
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
    Left = 336
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
    Left = 336
    Top = 44
    Width = 75
    Height = 25
    Caption = '&Modify'
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
    Left = 336
    Top = 133
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
    Left = 336
    Top = 165
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
end
