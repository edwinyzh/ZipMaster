object Mainform: TMainform
  Left = 200
  Top = 197
  Width = 604
  Height = 417
  Caption = 'Delphi Zip Merge Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 89
    Align = alTop
    TabOrder = 0
    object Panel1: TPanel
      Left = 484
      Top = 1
      Width = 103
      Height = 87
      Align = alRight
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label1: TLabel
        Left = 64
        Top = 62
        Width = 23
        Height = 15
        Caption = 'files'
      end
      object FilesLabel: TLabel
        Left = 24
        Top = 63
        Width = 7
        Height = 15
        Caption = '0'
      end
      object TimeLabel: TLabel
        Left = 64
        Top = 41
        Width = 31
        Height = 15
        Caption = '00:00'
      end
      object Label4: TLabel
        Left = 6
        Top = 41
        Width = 31
        Height = 15
        Caption = 'Time:'
      end
      object CloseBut: TButton
        Left = 9
        Top = 8
        Width = 85
        Height = 27
        Cancel = True
        Caption = 'E&xit'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = CloseButClick
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 483
      Height = 87
      Align = alClient
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object ZipFName: TLabel
        Left = 53
        Top = 64
        Width = 334
        Height = 14
        AutoSize = False
        Caption = '<none>'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 15
        Top = 64
        Width = 32
        Height = 14
        Caption = 'Zipfile:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 9
        Top = 9
        Width = 476
        Height = 47
      end
      object MsgBut: TButton
        Left = 376
        Top = 20
        Width = 101
        Height = 24
        Caption = '&Show Msgs'
        TabOrder = 0
        OnClick = MsgButClick
      end
      object MergeBut: TButton
        Left = 255
        Top = 20
        Width = 101
        Height = 25
        Caption = 'Merge'
        TabOrder = 1
        OnClick = MergeButClick
      end
      object NewZipBut: TButton
        Left = 135
        Top = 20
        Width = 96
        Height = 26
        Caption = '&New Zipfile'
        TabOrder = 2
        OnClick = NewZipButClick
      end
      object ZipOpenBut: TButton
        Left = 15
        Top = 20
        Width = 96
        Height = 25
        Caption = '&Open Zip'
        TabOrder = 3
        OnClick = ZipOpenButClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 548
    Top = 321
  end
  object MainMenu1: TMainMenu
    Left = 468
    Top = 321
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Project1: TMenuItem
      Caption = '&View'
      object Showlasterror1: TMenuItem
        Caption = 'Last error'
        OnClick = Showlasterror1Click
      end
      object Zipcomment1: TMenuItem
        Caption = 'Zip comment'
        OnClick = Zipcomment1Click
      end
    end
    object MergeOptions1: TMenuItem
      Caption = 'Merge Options'
      object OptConfirm: TMenuItem
        Tag = 1
        Caption = 'Confirm'
        RadioItem = True
      end
      object OptAlways: TMenuItem
        Tag = 2
        Caption = 'Always'
        RadioItem = True
      end
      object OptNewer: TMenuItem
        Tag = 3
        Caption = 'Newer'
        Checked = True
        RadioItem = True
      end
      object OptOlder: TMenuItem
        Tag = 4
        Caption = 'Older'
        RadioItem = True
      end
      object OptNever: TMenuItem
        Tag = 5
        Caption = 'Never'
        RadioItem = True
      end
      object OptRename: TMenuItem
        Tag = 6
        Caption = 'Rename'
        RadioItem = True
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Latesttime1: TMenuItem
        Caption = 'Latest time'
      end
      object ForceDestination1: TMenuItem
        Caption = 'Force Destination'
      end
      object Writesafe1: TMenuItem
        Caption = 'Write safe'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Verbose1: TMenuItem
        Caption = 'Verbose'
      end
      object Trace1: TMenuItem
        Caption = 'Trace'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Unattended1: TMenuItem
        Caption = 'Unattended'
      end
    end
  end
  object ImageList1: TImageList
    Left = 512
    Top = 321
  end
end
