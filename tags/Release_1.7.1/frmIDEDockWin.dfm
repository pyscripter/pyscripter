object IDEDockWindow: TIDEDockWindow
  Left = 356
  Top = 263
  Width = 237
  Height = 436
  BorderStyle = bsSizeToolWin
  Caption = 'IDE Dock Window'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    229
    410)
  PixelsPerInch = 96
  TextHeight = 13
  object FGPanel: TPanel
    Left = 0
    Top = 0
    Width = 227
    Height = 396
    Anchors = []
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    OnEnter = FGPanelEnter
    OnExit = FGPanelExit
  end
  object DockClient: TJvDockClient
    LRDockWidth = 220
    TBDockHeight = 220
    DirectDrag = False
    DockStyle = PyIDEMainForm.JvDockVSNetStyleTBX
    OnTabHostFormCreated = DockClientTabHostFormCreated
    Left = 8
    Top = 10
  end
end
