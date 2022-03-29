object Extract: TExtract
  Left = 94
  Top = 145
  Width = 485
  Height = 385
  Caption = 'Select Extract Directory'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 333
    Height = 351
    Align = alClient
    TabOrder = 0
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 25
      Width = 331
      Height = 325
      Align = alClient
      ItemHeight = 16
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 331
      Height = 24
      Align = alTop
      TabOrder = 1
      object DriveComboBox1: TDriveComboBox
        Left = 2
        Top = 0
        Width = 325
        Height = 22
        DirList = DirectoryListBox1
        TabOrder = 0
        TextCase = tcUpperCase
      end
    end
  end
  object Panel2: TPanel
    Left = 333
    Top = 0
    Width = 144
    Height = 351
    Align = alRight
    TabOrder = 1
    object OKBut: TButton
      Left = 36
      Top = 252
      Width = 75
      Height = 25
      Caption = '&Ok'
      TabOrder = 0
      OnClick = OKButClick
    end
    object CancelBut: TButton
      Left = 36
      Top = 297
      Width = 75
      Height = 24
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = CancelButClick
    end
    object RadioGroup1: TRadioGroup
      Left = 14
      Top = 20
      Width = 116
      Height = 81
      Caption = 'Expand ZIP Dirs'
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 2
    end
    object RadioGroup2: TRadioGroup
      Left = 12
      Top = 124
      Width = 121
      Height = 77
      Caption = 'OverWrite Existing'
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 3
    end
  end
end
