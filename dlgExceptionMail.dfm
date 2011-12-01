inherited ExceptionDialogMail: TExceptionDialogMail
  Left = 310
  Top = 255
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  ClientHeight = 245
  ClientWidth = 422
  Constraints.MinWidth = 200
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  ExplicitWidth = 438
  ExplicitHeight = 283
  DesignSize = (
    422
    245)
  PixelsPerInch = 96
  TextHeight = 13
  object BevelDetails: TBevel
    Left = 3
    Top = 91
    Width = 412
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 422
  end
  object SendBtn: TSpTBXButton
    Left = 341
    Top = 32
    Width = 75
    Height = 25
    Hint = 'Send bug report using default mail client'
    Caption = '&Send'
    Anchors = [akTop, akRight]
    TabOrder = 0
    OnClick = SendBtnClick
  end
  object OkBtn: TSpTBXButton
    Left = 342
    Top = 4
    Width = 75
    Height = 25
    Caption = '&OK'
    Anchors = [akTop, akRight]
    TabOrder = 2
    Default = True
    ModalResult = 1
  end
  object DetailsBtn: TSpTBXButton
    Left = 342
    Top = 60
    Width = 75
    Height = 25
    Hint = 'Show or hide additional information|'
    Caption = '&Details'
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 3
    OnClick = DetailsBtnClick
  end
  object DetailsMemo: TMemo
    Left = 4
    Top = 101
    Width = 411
    Height = 137
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WantReturns = False
    WordWrap = False
  end
  object TextMemo: TMemo
    Left = 56
    Top = 8
    Width = 271
    Height = 75
    Hint = 'Use Ctrl+C to copy the report to the clipboard'
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Ctl3D = True
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
    WantReturns = False
  end
end
