object JvDirectoryListDialog: TJvDirectoryListDialog
  Left = 206
  Top = 99
  ActiveControl = AddBtn
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
  object AddBtn: TButton
    Left = 336
    Top = 12
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add...'
    TabOrder = 0
    OnClick = AddBtnClick
    ExplicitLeft = 298
  end
  object RemoveBtn: TButton
    Left = 336
    Top = 76
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Remove'
    TabOrder = 2
    OnClick = RemoveBtnClick
    ExplicitLeft = 298
  end
  object ModifyBtn: TButton
    Left = 336
    Top = 44
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Modify'
    TabOrder = 1
    OnClick = ModifyBtnClick
    ExplicitLeft = 298
  end
  object OKBtn: TButton
    Left = 336
    Top = 133
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    ExplicitLeft = 298
  end
  object CancelBtn: TButton
    Left = 336
    Top = 165
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ExplicitLeft = 298
  end
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
    ExplicitWidth = 284
  end
end
