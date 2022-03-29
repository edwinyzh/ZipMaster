object Mainform: TMainform
  Left = 312
  Top = 277
  ActiveControl = ZipOpenBut
  Caption = 'Delphi Zip Demo1'
  ClientHeight = 367
  ClientWidth = 604
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 604
    Height = 125
    Align = alTop
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 124
      Height = 123
      Align = alLeft
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object DeleteZipBut: TButton
        Left = 15
        Top = 83
        Width = 96
        Height = 26
        Caption = 'Destroy Zipfile'
        TabOrder = 2
        OnClick = DeleteZipButClick
      end
      object NewZipBut: TButton
        Left = 15
        Top = 48
        Width = 96
        Height = 26
        Caption = '&New Zipfile'
        TabOrder = 1
        OnClick = NewZipButClick
      end
      object ZipOpenBut: TButton
        Left = 15
        Top = 13
        Width = 96
        Height = 25
        Caption = '&Open Zip'
        TabOrder = 0
        OnClick = ZipOpenButClick
      end
    end
    object Panel1: TPanel
      Left = 500
      Top = 1
      Width = 103
      Height = 123
      Align = alRight
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Bevel2: TBevel
        Left = 9
        Top = 96
        Width = 85
        Height = 21
      end
      object Label1: TLabel
        Left = 64
        Top = 99
        Width = 23
        Height = 15
        Caption = 'files'
      end
      object FilesLabel: TLabel
        Left = 27
        Top = 99
        Width = 7
        Height = 15
        Caption = '0'
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
      object VerboseCB: TCheckBox
        Left = 12
        Top = 39
        Width = 77
        Height = 17
        Caption = 'Verbose'
        TabOrder = 1
        OnClick = VerboseCBClick
      end
      object TraceCB: TCheckBox
        Left = 12
        Top = 57
        Width = 73
        Height = 17
        Caption = 'Trace'
        TabOrder = 2
        OnClick = TraceCBClick
      end
      object UnattendedCB: TCheckBox
        Left = 12
        Top = 75
        Width = 85
        Height = 17
        Caption = 'Unattended'
        TabOrder = 3
        OnClick = UnattendedCBClick
      end
    end
    object Panel4: TPanel
      Left = 125
      Top = 1
      Width = 375
      Height = 123
      Align = alClient
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object ZipFName: TLabel
        Left = 42
        Top = 100
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
        Left = 6
        Top = 100
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
        Width = 369
        Height = 88
      end
      object TimeLabel: TLabel
        Left = 296
        Top = 66
        Width = 31
        Height = 15
        Caption = '00:00'
      end
      object Label4: TLabel
        Left = 296
        Top = 46
        Width = 31
        Height = 15
        Caption = 'Time:'
      end
      object DeleteBut: TButton
        Left = 18
        Top = 68
        Width = 101
        Height = 24
        Caption = '&Delete From Zip'
        TabOrder = 4
        OnClick = DeleteButClick
      end
      object AddBut: TButton
        Left = 18
        Top = 14
        Width = 101
        Height = 24
        Caption = '&Add  to Zip'
        TabOrder = 0
        OnClick = AddButClick
      end
      object ExtractBut: TButton
        Left = 18
        Top = 41
        Width = 101
        Height = 24
        Caption = '&Extract from Zip'
        TabOrder = 1
        OnClick = ExtractButClick
      end
      object TestBut: TButton
        Left = 141
        Top = 68
        Width = 101
        Height = 24
        Caption = '&Test Zip'
        TabOrder = 3
        OnClick = TestButClick
      end
      object MsgBut: TButton
        Left = 264
        Top = 14
        Width = 101
        Height = 24
        Caption = '&Show Msgs'
        TabOrder = 5
        OnClick = MsgButClick
      end
      object ConvertBut: TButton
        Left = 141
        Top = 14
        Width = 101
        Height = 24
        Caption = '&Convert to EXE'
        TabOrder = 2
        OnClick = ConvertButClick
      end
      object RenameBut: TButton
        Left = 141
        Top = 41
        Width = 101
        Height = 24
        Caption = '&Rename'
        TabOrder = 6
        OnClick = RenameButClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 572
    Top = 321
  end
  object MainMenu1: TMainMenu
    Left = 500
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
      object DLLversioninfo1: TMenuItem
        Caption = 'Version info'
        OnClick = DLLversioninfo1Click
      end
    end
  end
  object ImageList1: TImageList
    Left = 536
    Top = 321
  end
end
