object ZipForm: TZipForm
  Left = 88
  Top = 131
  Width = 591
  Height = 389
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  ActiveControl = Button2
  Caption = 'Zip File Contents'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -15
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 17
  object TButton
    Left = 435
    Top = 12
    Width = 85
    Height = 29
    Caption = '&Exit'
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 583
    Height = 46
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object ZipFNameLabel: TLabel
      Left = 22
      Top = 7
      Width = 100
      Height = 16
      Caption = 'ZipFNameLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 20
      Top = 26
      Width = 266
      Height = 17
      Caption = 'Click on Column Name to sort by column'
    end
    object Button2: TButton
      Left = 495
      Top = 9
      Width = 78
      Height = 26
      Caption = '&Exit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'System'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button2Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 416
    Top = 7
  end
end
