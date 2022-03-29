object Extract: TExtract
  Left = 249
  Top = 171
  Width = 480
  Height = 407
  Caption = 'Select Extract Directory'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 380
    Align = alClient
    TabOrder = 0
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 20
      Width = 343
      Height = 359
      Align = alClient
      ItemHeight = 16
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 343
      Height = 19
      Align = alTop
      TabOrder = 1
      object DriveComboBox1: TDriveComboBox
        Left = 2
        Top = 0
        Width = 259
        Height = 19
        DirList = DirectoryListBox1
        TabOrder = 0
        TextCase = tcUpperCase
      end
    end
  end
  object Panel2: TPanel
    Left = 345
    Top = 0
    Width = 127
    Height = 380
    Align = alRight
    TabOrder = 1
    object OKBut: TButton
      Left = 29
      Top = 247
      Width = 61
      Height = 20
      Caption = '&Ok'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = OKButClick
    end
    object CancelBut: TButton
      Left = 29
      Top = 283
      Width = 61
      Height = 20
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 3
      OnClick = CancelButClick
    end
    object RadioGroup1: TRadioGroup
      Left = 10
      Top = 13
      Width = 103
      Height = 59
      Caption = 'Expand ZIP Dirs'
      ItemIndex = 0
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 0
    end
    object RadioGroup2: TRadioGroup
      Left = 10
      Top = 88
      Width = 103
      Height = 59
      Caption = 'OverWrite Existing'
      ItemIndex = 0
      Items.Strings = (
        'No'
        'Yes')
      TabOrder = 1
    end
  end
end
