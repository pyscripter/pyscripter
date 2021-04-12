inherited AboutBox: TAboutBox
  Left = 365
  Top = 155
  BorderIcons = [biSystemMenu]
  Caption = 'About PyScripter'
  ClientHeight = 353
  ClientWidth = 534
  KeyPreview = True
  OldCreateOrder = True
  StyleElements = [seFont, seBorder]
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
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
            Zoom = 100
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
        Zoom = 100
      end
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 534
      Height = 328
      Caption = 'About'
      ImageIndex = -1
      DesignSize = (
        534
        328)
      TabItem = 'tbAbout'
      object Panel1: TPanel
        AlignWithMargins = True
        Left = 24
        Top = 24
        Width = 481
        Height = 281
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Anchors = [akLeft, akTop, akRight, akBottom]
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
          481
          281)
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
          Height = 88
          Margins.Left = 10
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Anchors = [akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'A freeware, open source Python scripting integrated development ' +
            'environment created with the ambition to bring to the Python com' +
            'munity the quality and functionality available in commercial IDE' +
            's available for other languages.'
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
          Width = 150
          Height = 150
          AutoSize = False
          Proportional = True
          SVGText = 
            '<svg id="acfdf7c2-779f-498a-9313-fcf213028fbc" data-name="Layer ' +
            '1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 35.95 35.95">' +
            '<g id="a52711da-1cc0-4b2c-afbf-708a46579e22" data-name="b67fef1f' +
            '-d78f-41b9-babd-ba78b7afaa03"><path d="M11.1,29.86h9.44c5.72,0,5' +
            '.77-2.19,5.85-4.1a19,19,0,0,0,0-2.61,5.05,5.05,0,0,0-1.27-2.86,1' +
            '.67,1.67,0,0,0-.23-.23,1.74,1.74,0,0,0-.82-.47c-.47.19-3.65-.36-' +
            '4.49-.25Z" fill="#7f7f19"/><polyline points="27.27 3.91 24.72 2.' +
            '25 24.29 2.79 6.71 28.72 9.4 30.53" fill="#f4a800"/><polyline po' +
            'ints="9.25 30.42 12.26 32.48 24.98 13.7 30.22 5.96 27.3 3.91" fi' +
            'll="#dd910a"/><polyline points="5.72 35.52 6.71 28.72 9.23 30.43' +
            '" fill="#c99052"/><polyline points="9.23 30.43 12.26 32.48 5.72 ' +
            '35.52" fill="#b77f48"/><path d="M29.18,1.46l1.06.78A2.19,2.19,0,' +
            '0,1,30.7,5.3l0,.06L28.8,7.88h-.09l-3-2.18" fill="#db7575"/><path' +
            ' d="M26,5.86,23.36,4.09l.09-.15L25.4,1.32A2.1,2.1,0,0,1,28.32.88' +
            'l.86.64" fill="#ed8585"/><path d="M24.1,19.64A3.16,3.16,0,0,0,26' +
            ',16.18v-5.7c0-3.71.12-8.68-7.32-8.68-2.45,0-4.78.33-6.75,3-1.62,' +
            '2.19-1.26,5.85-1.2,5.92h8.56a.65.65,0,0,1,.65.64h0v1.19L9.81,12.' +
            '39a5.06,5.06,0,0,0-5.06,5.05v8.29a3.72,3.72,0,0,0,1.89,3.16s3.79' +
            '-5.44,5.17-7.65A2.52,2.52,0,0,1,14.34,20C19.44,20.14,22.2,20.33,' +
            '24.1,19.64Z" fill="#939914"/><path d="M18.18,6.2v.93A1.14,1.14,0' +
            ',0,1,17,8.26h0a1.13,1.13,0,0,1-1.15-1.12h0" fill="#202d81"/><pat' +
            'h d="M15.89,7.13V6.2A1.15,1.15,0,0,1,17,5.06h0A1.14,1.14,0,0,1,1' +
            '8.18,6.2" fill="#25378e"/><polyline points="5.72 35.52 5.98 33.7' +
            '7 6.6 34.27" fill="#666"/><polyline points="6.6 34.27 7.25 34.82' +
            ' 5.72 35.52" fill="#565656"/><path d="M13.19,19.91a15.46,15.46,0' +
            ',0,1,4.4.13l3.92.07a37.62,37.62,0,0,1-4.86.19,9.41,9.41,0,0,0-4.' +
            '84.95A2.1,2.1,0,0,1,13.19,19.91Z" fill="#686810"/><path d="M14.0' +
            '5,29.86h.32s3.18-5.16,6.33-6.64a13.19,13.19,0,0,0,3.22-2.59c.66-' +
            '.64.58-.86.58-.86l-.31-.15a11,11,0,0,1-3.54.54Z" fill="#686810"/' +
            '><path d="M19.38,10.73a1.85,1.85,0,0,1,2,1.73v.09H20a10.05,10.05' +
            ',0,0,0-.08-1.26A.67.67,0,0,0,19.38,10.73Z" fill="#686810"/></g><' +
            '/svg>'
          Anchors = [akLeft, akTop, akRight, akBottom]
        end
      end
    end
  end
end
