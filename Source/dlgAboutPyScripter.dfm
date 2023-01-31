inherited AboutBox: TAboutBox
  Left = 365
  Top = 155
  BorderIcons = [biSystemMenu]
  Caption = 'About PyScripter'
  ClientHeight = 353
  ClientWidth = 534
  KeyPreview = True
  StyleElements = [seFont, seBorder]
  OnKeyPress = FormKeyPress
  TextHeight = 25
  object SpTBXTabControl: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 534
    Height = 353
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ActiveTabIndex = 0
    TabBackgroundBorders = True
    TabPosition = ttpBottom
    HiddenItems = <>
    object tbAbout: TSpTBXTabItem
      Caption = 'About'
      Checked = True
    end
    object tbCredits: TSpTBXTabItem
      Caption = 'Credits'
    end
    object tbLinks: TSpTBXTabItem
      Caption = 'Links'
    end
    object SpTBXTabSheet2: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 534
      Height = 328
      Caption = 'Credits'
      ImageIndex = -1
      TabItem = 'tbCredits'
      object ScrollBox: TSpTBXPageScroller
        Left = 2
        Top = 4
        Width = 528
        Height = 324
        Align = alClient
        Color = clBtnFace
        Ctl3D = False
        ParentColor = False
        ParentCtl3D = False
        TabOrder = 0
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 528
          Height = 89
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object reCredits: TRichEdit
            Left = 0
            Top = 0
            Width = 528
            Height = 89
            Align = alTop
            BorderStyle = bsNone
            Font.Charset = ANSI_CHARSET
            Font.Color = clBtnText
            Font.Height = -12
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnResizeRequest = reCreditsResizeRequest
          end
        end
      end
    end
    object SpTBXTabSheet3: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 534
      Height = 328
      Caption = 'Links'
      ImageIndex = -1
      TabItem = 'tbLinks'
      object reLinks: TRichEdit
        Left = 2
        Top = 4
        Width = 528
        Height = 324
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 534
      Height = 328
      Caption = 'About'
      ImageIndex = -1
      TabItem = 'tbAbout'
      object Panel1: TPanel
        AlignWithMargins = True
        Left = 22
        Top = 24
        Width = 488
        Height = 284
        Margins.Left = 20
        Margins.Top = 20
        Margins.Right = 20
        Margins.Bottom = 20
        Align = alClient
        DoubleBuffered = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBtnText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        ParentBackground = False
        ParentDoubleBuffered = False
        ParentFont = False
        ShowCaption = False
        TabOrder = 0
        OnClick = Panel1Click
        DesignSize = (
          488
          284)
        object Copyright: TLabel
          Left = 233
          Top = 102
          Width = 4
          Height = 19
          Caption = ' '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 16746564
          Font.Height = -16
          Font.Name = 'Arial'
          Font.Style = [fsItalic]
          ParentFont = False
          StyleElements = [seClient, seBorder]
          OnClick = Panel1Click
        end
        object Version: TLabel
          Left = 233
          Top = 122
          Width = 113
          Height = 24
          Caption = 'Version 1.3'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 16746564
          Font.Height = -21
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsItalic]
          ParentFont = False
          StyleElements = [seClient, seBorder]
          OnClick = Panel1Click
        end
        object ProductName: TLabel
          Left = 233
          Top = 25
          Width = 160
          Height = 36
          Caption = 'PyScripter'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 16746564
          Font.Height = -32
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsItalic]
          ParentFont = False
          Transparent = True
          StyleElements = [seClient, seBorder]
          OnClick = Panel1Click
        end
        object Comments: TLabel
          AlignWithMargins = True
          Left = 25
          Top = 192
          Width = 433
          Height = 91
          Margins.Left = 10
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Anchors = [akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'A free and open source Python scripting integrated development e' +
            'nvironment created with the ambition to bring to the Python comm' +
            'unity the quality and functionality available in commercial IDEs' +
            ' available for other languages.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -12
          Font.Name = 'MS Shell Dlg 2'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          OnClick = Panel1Click
        end
        object SVGIconImage1: TSVGIconImage
          Left = 25
          Top = 25
          Width = 157
          Height = 153
          AutoSize = False
          SVGText = 
            '<svg id="b1a03016-5506-42bd-8c49-4cc7f72dfa4b" data-name="acfdf7' +
            'c2-779f-498a-9313-fcf213028fbc" xmlns="http://www.w3.org/2000/sv' +
            'g" viewBox="0 0 36 36"><g id="b82c0a2f-d2a9-42fd-90bf-01ead63058' +
            'c1" data-name="a52711da-1cc0-4b2c-afbf-708a46579e22"><path d="M1' +
            '1.13,30h9.43c5.73,0,5.83-2.21,5.93-4.12a11.39,11.39,0,0,0,0-2.61' +
            ',5.12,5.12,0,0,0-1.31-2.91l-.2-.2a2.18,2.18,0,0,0-.8-.5c-.5.2-3.' +
            '62-.4-4.52-.3Z" fill="#33bc4a"/><polyline points="27.39 3.91 24.' +
            '62 2.14 6.71 28.81 9.22 30.52" fill="#ffce00"/><polyline points=' +
            '"9.22 30.52 12.33 32.63 25.08 13.75 30.3 6.02 27.39 3.91" fill="' +
            '#dd910a"/><polyline points="5.7 35.64 6.71 28.81 9.22 30.52" fil' +
            'l="#f79c41"/><polyline points="9.22 30.52 12.33 32.63 5.7 35.64"' +
            ' fill="#e58833"/><path d="M29.3,1.5l1.1.81a2.23,2.23,0,0,1,.51,3' +
            '.11L29,8h0L25.89,5.82" fill="#c93636"/><path d="M26,5.92,23.26,4' +
            '.07l.13-.26L25.3,1.2A2.09,2.09,0,0,1,28.21.8l1.09.7" fill="#e244' +
            '44"/><path d="M24.18,19.68c1.4-.61,1.91-1.51,1.91-3.52V10.44c0-3' +
            '.72.1-8.74-7.33-8.74-2.51,0-4.82.31-6.83,3a9.12,9.12,0,0,0-1.21,' +
            '5.92h8.64a.58.58,0,0,1,.6.6h0v1.21H9.82A5.14,5.14,0,0,0,4.7,17.5' +
            '7V25.9a3.72,3.72,0,0,0,1.91,3.22l0,0,0-.33c.53-.75,3.66-5.28,5-7' +
            '.31a2.6,2.6,0,0,1,2.67-1.22C19.46,20.18,22.27,20.38,24.18,19.68Z' +
            '" fill="#33bc4a"/><polyline points="5.7 35.65 5.96 33.92 6.58 34' +
            '.43" fill="#666"/><polyline points="6.58 34.43 7.3 34.92 5.7 35.' +
            '65" fill="#565656"/><path d="M13.46,19.56c1.26-.34,2.22-.2,4.38.' +
            '17a22.31,22.31,0,0,0,4.39.5c-1.61.3-1.88.57-5.54.16-2-.17-3.1-.3' +
            '8-4.83.77A2.8,2.8,0,0,1,13.46,19.56Z" fill="#239923"/><path d="M' +
            '14.09,30h.27s2.69-5,6.41-6.63A8.79,8.79,0,0,0,24,20.78a6.29,6.29' +
            ',0,0,0,.61-.9s0,0-.14-.09a2.43,2.43,0,0,0-.27-.11,7.93,7.93,0,0,' +
            '1-3.52.6Z" fill="#239923"/><path d="M19.41,10.63a1.79,1.79,0,0,1' +
            ',2,1.71h0v.1H20a5.73,5.73,0,0,0-.11-1.31A.64.64,0,0,0,19.41,10.6' +
            '3Z" fill="#239923"/><rect x="15.95" y="5.12" width="2.25" height' +
            '="3.11" rx="1.13" fill="#1c66af"/></g></svg>'
          Anchors = [akLeft, akTop, akRight, akBottom]
        end
      end
    end
  end
end
