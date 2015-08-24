inherited ConfigureTools: TConfigureTools
  Left = 342
  Top = 215
  HelpContext = 710
  BorderIcons = [biSystemMenu]
  Caption = 'Configure Tools'
  ClientHeight = 267
  ClientWidth = 301
  OldCreateOrder = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 307
  ExplicitHeight = 296
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 0
    Width = 301
    Height = 267
    Caption = 'SpTBXPanel1'
    Align = alClient
    TabOrder = 0
    TBXStyleBackground = True
    DesignSize = (
      301
      267)
    object AddBtn: TSpTBXButton
      Left = 200
      Top = 12
      Width = 90
      Height = 25
      Caption = '&Add...'
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = AddBtnClick
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
    end
  end
end
