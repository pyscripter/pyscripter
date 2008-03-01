object IDEDockWindow: TIDEDockWindow
  Left = 356
  Top = 263
  BorderStyle = bsSizeToolWin
  Caption = 'IDE Dock Window'
  ClientHeight = 408
  ClientWidth = 227
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poDesigned
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  DesignSize = (
    227
    408)
  PixelsPerInch = 96
  TextHeight = 13
  object FGPanel: TPanel
    Left = -1
    Top = -1
    Width = 227
    Height = 396
    Anchors = []
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 0
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
